{-# LANGUAGE ScopedTypeVariables #-}
module Bundle (Bundle(..), Header(..), get, decompress) where

import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import qualified Data.Binary.Get as B
import qualified System.IO.Temp as Temp

data Bundle = Bundle
  { header :: Header
  , blocks :: [ByteString]
  }

data Header = Header
  { uncompressedSize :: Int64
  , totalPayloadSize :: Int64
  , firstFileEncode :: Int
  , blockCount :: Int
  , uncompressedBlockGranularity :: Int
  , blockSizes :: [Int]
  }
  deriving (Show)

get :: ByteString -> Either (ByteString, B.ByteOffset, String) (ByteString, B.ByteOffset, Bundle)
get = B.runGetOrFail parse

-- TODO streaming?
decompress :: Bundle -> IO [ByteString]
decompress _bundle =
  Temp.withSystemTempDirectory "poe-hs-" $ \dirpath -> do
    Temp.withTempFile dirpath "poe-hs-" $ \filepath _fd -> do
      putStrLn filepath
      return []

parse :: B.Get Bundle
parse = do
  -- hs binary parsing overview: https://wiki.haskell.org/Dealing_with_binary_data
  -- ggpk bundle format: https://github.com/poe-tool-dev/ggpk.discussion/wiki/Bundle-scheme
  _uncompressedSize32 <- B.getWord32le
  _totalPayloadSize32 <- B.getWord32le
  _headPayloadSize <- B.getWord32le
  header' <- parseHeader
  blocks' <- parseBlocks $ blockSizes header'
  -- head.payload:
  return $ Bundle header' blocks'

parseBlocks :: [Int] -> B.Get [ByteString]
parseBlocks [] = return []
parseBlocks (size : next) = do
  block <- B.getLazyByteString (fromIntegral size)
  tail' <- parseBlocks next
  return $ block : tail'

parseHeader :: B.Get Header
parseHeader = do
  firstFileEncode' <- fmap fromIntegral B.getWord32le
  _unk10 <- B.getWord32le
  uncompressedSize' <- fmap fromIntegral B.getWord64le
  totalPayloadSize' <- fmap fromIntegral B.getWord64le
  blockCount' <- fmap fromIntegral B.getWord32le
  uncompressedBlockGranularity' <- fmap fromIntegral B.getWord32le
  _unk28 <- parseList 4 B.getWord32le
  blockSizes' <- parseList blockCount' $ fmap fromIntegral B.getWord32le
  return $ Header
    firstFileEncode'
    uncompressedSize'
    totalPayloadSize'
    blockCount'
    uncompressedBlockGranularity'
    blockSizes'

parseList :: Int -> B.Get a -> B.Get [a]
parseList len getfn
  | len <= 0 = return []
  | otherwise = do
      val <- getfn
      tail' <- parseList (len-1) getfn
      return $ val : tail'