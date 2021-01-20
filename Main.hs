{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified System.FilePath
import System.Exit (die)
import qualified Data.List
import qualified System.Environment
import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Data.Binary.Get as B
import Data.Word (Word32)
import Data.Int (Int64)
import System.Process

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

main :: IO ()
main = do
  (argv :: [String]) <- System.Environment.getArgs
  case argv of
    [] -> die "usage: poe-hs <GGPK_PATH>"
    (depotPath : _) -> do
      let (indexPath :: String) = System.FilePath.joinPath [depotPath, "Bundles2", "_.index.bin"]
      (indexBin :: ByteString) <- ByteString.readFile $ indexPath
      case B.runGetOrFail parseIndex indexBin of
        Left (_, _, err) -> die err
        Right (bytesLeft, _, (index :: Bundle))
          | bytesLeft /= ByteString.empty -> die "parsed bundle, but had bytes left over"
          | otherwise -> do
            putStrLn $ show $ header index

parseIndex :: B.Get Bundle
parseIndex = do
  -- hs binary parsing overview: https://wiki.haskell.org/Dealing_with_binary_data
  -- ggpk bundle format: https://github.com/poe-tool-dev/ggpk.discussion/wiki/Bundle-scheme
  uncompressedSize32 <- fmap fromIntegral B.getWord32le
  totalPayloadSize32 <- fmap fromIntegral B.getWord32le
  headPayloadSize <- fmap fromIntegral B.getWord32le
  header <- parseHeader
  blocks <- parseBlocks $ blockSizes header
  -- head.payload:
  return $ Bundle header []

parseBlocks :: [Int] -> B.Get [ByteString]
parseBlocks [] = return []
parseBlocks (size : next) = do
  block <- B.getLazyByteString (fromIntegral size)
  tail <- parseBlocks next
  return $ block : tail

parseHeader :: B.Get Header
parseHeader = do
  firstFileEncode <- fmap fromIntegral B.getWord32le
  unk10 <- fmap fromIntegral B.getWord32le
  uncompressedSize <- fmap fromIntegral B.getWord64le
  totalPayloadSize <- fmap fromIntegral B.getWord64le
  blockCount <- fmap fromIntegral B.getWord32le
  uncompressedBlockGranularity <- fmap fromIntegral B.getWord32le
  (unk28 :: [Word32]) <- parseList 4 B.getWord32le
  blockSizes <- parseList blockCount $ fmap fromIntegral B.getWord32le
  return $ Header
    firstFileEncode
    uncompressedSize
    totalPayloadSize
    blockCount
    uncompressedBlockGranularity
    blockSizes

parseList :: Int -> B.Get a -> B.Get [a]
parseList len getfn
  | len <= 0 = return []
  | otherwise = do
      val <- getfn
      tail <- parseList (len-1) getfn
      return $ val : tail
