{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified System.FilePath
import System.Exit (die)
import qualified System.Environment
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson

import qualified Bundle
import Bundle (Bundle)
import qualified IndexBundle
import IndexBundle (IndexBundle)

main :: IO ()
main = do
  (argv :: [String]) <- System.Environment.getArgs
  case argv of
    (depotPath : outputPath : []) -> do
      let (indexPath :: String) = System.FilePath.joinPath [depotPath, "Bundles2", "_.index.bin"]
      (indexBin :: ByteString) <- ByteString.readFile $ indexPath
      case Bundle.get indexBin of
        Left (_, _, err) -> die $ "bundle.get: "++err
        Right (bytesLeft, _, (indexBundle :: Bundle))
          | bytesLeft /= ByteString.empty -> die "parsed bundle, but had bytes left over"
          | otherwise -> do
            -- _ <- Bundle.decompressTo outputPath indexBundle
            indexBytes <- Bundle.decompressTo outputPath indexBundle
            putStrLn $ outputPath
            putStrLn $ show $ Bundle.header indexBundle
            case IndexBundle.get indexBytes of
              Left (_, _, err) -> die $ "IndexBundle.get: "++err
              Right (_bytesLeft', _, (indexBody :: IndexBundle))
                -- | bytesLeft /= ByteString.empty -> die "parsed IndexBundle, but had bytes left over"
                | otherwise -> do
                  putStrLn $ Data.ByteString.Lazy.Char8.unpack $ Data.Aeson.encode indexBody
    _ -> die "usage: poe-hs <GGPK_PATH> <OUTPUT_PATH>"
