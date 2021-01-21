{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified System.FilePath
import System.Exit (die)
import qualified System.Environment
import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Bundle
import Bundle (Bundle)

main :: IO ()
main = do
  (argv :: [String]) <- System.Environment.getArgs
  case argv of
    (depotPath : outputPath : []) -> do
      let (indexPath :: String) = System.FilePath.joinPath [depotPath, "Bundles2", "_.index.bin"]
      (indexBin :: ByteString) <- ByteString.readFile $ indexPath
      case Bundle.get indexBin of
        Left (_, _, err) -> die err
        Right (bytesLeft, _, (index :: Bundle))
          | bytesLeft /= ByteString.empty -> die "parsed bundle, but had bytes left over"
          | otherwise -> do
            _ <- Bundle.decompress outputPath index
            putStrLn $ outputPath
            putStrLn $ show $ Bundle.header index
    _ -> die "usage: poe-hs <GGPK_PATH> <OUTPUT_PATH>"
