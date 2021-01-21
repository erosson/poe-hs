{-# LANGUAGE ScopedTypeVariables #-}
module Bundle (Bundle(..), Header(..), get, decompress) where

import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
-- import qualified Data.Binary as Binary
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as BP
-- import qualified System.IO.Temp as Temp
import qualified System.Process as Process
import System.Exit (die, ExitCode(ExitFailure, ExitSuccess))
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Text.Printf as Printf

data Bundle = Bundle
  { header :: Header
  , blocks :: [ByteString]
  }

data Header = Header
  { firstFileEncode :: Int
  , uncompressedSize :: Int64
  , compressedSize :: Int64 -- aka totalPayloadSize
  , blockCount :: Int
  , uncompressedBlockGranularity :: Int
  , blockSizes :: [Int]
  }
  deriving (Show)

-- | After reading a bundle, decompress it using ooz.exe/libooz.
-- Decompressing the entire bundle in memory will fail for large bundles - but
-- Haskell's lazy, so we can iterate even a large list of results. (I think.)
decompress :: FilePath -> Bundle -> IO [ByteString]
decompress outputDir bundle = do
  Directory.createDirectoryIfMissing True outputDir
  mapM (decompressBlock outputDir bundle) $ zipWith (,) [0..] $ blocks bundle

decompressBlock :: FilePath -> Bundle -> (Int, ByteString) -> IO ByteString
decompressBlock outputRoot bundle (blockNum, block) = do
  -- emptyTempFile does not delete the file, like withTempFile would do - but
  -- withTempFile would open the file, so Binary.encodeFile can't use it.
  -- withSystemTempDirectory above deletes it later anyway.
  let (rawDir :: FilePath) = FilePath.joinPath [outputRoot, "compressed"]
  let (outputDir :: FilePath) = FilePath.joinPath [outputRoot, "uncompressed"]
  let (basename :: String) = Printf.printf "ggpk-block-%04d" blockNum
  let (rawPath :: FilePath) = FilePath.joinPath [rawDir, basename ++ ".in"]
  let (outputPath :: FilePath) = FilePath.joinPath [outputDir, basename ++ ".out"]
  Directory.createDirectoryIfMissing True rawDir
  Directory.createDirectoryIfMissing True outputDir
  ByteString.writeFile rawPath $ BP.runPut $ putBlock bundle blockNum block
  cmd <- Process.readProcessWithExitCode "./ooz.exe" ["-d", rawPath, outputPath] ""
  case cmd of
    (ExitFailure code, stderr, stdout) ->
      die $ "ooz.exe failed, exit " ++ show code ++ ": \n" ++ stderr ++ "\n" ++ stdout
    (ExitSuccess, _stderr, _stdout) ->
      ByteString.readFile outputPath

-- | Write a block in the format expected by libooz/ooz.exe:
-- little-endian Int64 uncompressed-size, followed by the block.
-- Usually uncompressed-size is bundle.header.blockSize - except for the last
-- block, which probably doesn't divide evenly.
putBlock :: Bundle -> Int -> ByteString -> BP.Put
putBlock bundle blockNum block = do
  let (blockSize :: Int64) = fromIntegral $ uncompressedBlockGranularity $ header bundle
  let bundleSize = uncompressedSize $ header bundle
  let blockSize' = min blockSize $ bundleSize - fromIntegral blockNum * blockSize
  BP.putWord64le $ fromIntegral blockSize'
  BP.putByteString $ ByteString.toStrict block


-- | Read a compressed bundle file.
get :: ByteString -> Either (ByteString, B.ByteOffset, String) (ByteString, B.ByteOffset, Bundle)
get = B.runGetOrFail parse

parse :: B.Get Bundle
parse = do
  -- hs binary parsing overview: https://wiki.haskell.org/Dealing_with_binary_data
  -- ggpk bundle format: https://github.com/poe-tool-dev/ggpk.discussion/wiki/Bundle-scheme
  _uncompressedSize32 <- B.getWord32le
  _compressedSize32 <- B.getWord32le
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
  compressedSize' <- fmap fromIntegral B.getWord64le
  blockCount' <- fmap fromIntegral B.getWord32le
  uncompressedBlockGranularity' <- fmap fromIntegral B.getWord32le
  _unk28 <- parseList 4 B.getWord32le
  blockSizes' <- parseList blockCount' $ fmap fromIntegral B.getWord32le
  return $ Header
    firstFileEncode'
    uncompressedSize'
    compressedSize'
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
