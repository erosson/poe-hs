{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module IndexBundle (IndexBundle(..), get) where

import GHC.Generics
import Control.Monad (replicateM)
import qualified Data.Aeson
-- import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Data.Binary.Get as B

data IndexBundle = IndexBundle
  { bundleCount :: Int
  , bundleInfo :: [BundleInfo]
  }
  deriving (Show, Generic)
-- https://hackage.haskell.org/package/aeson-1.5.5.1/docs/Data-Aeson.html
instance Data.Aeson.ToJSON IndexBundle

type BundleInfo = (String, Int)

get :: ByteString -> Either (ByteString, B.ByteOffset, String) (ByteString, B.ByteOffset, IndexBundle)
get = B.runGetOrFail parse

parse :: B.Get IndexBundle
parse = do
  bundleCount' <- fmap fromIntegral B.getWord32le
  bundleInfo' <- replicateM bundleCount' $ do
    nameLen <- fmap fromIntegral B.getWord32le
    name <- fmap show $ B.getByteString nameLen
    size <- fmap fromIntegral B.getWord32le
    return ((name, size) :: BundleInfo)
  return $ IndexBundle
    bundleCount'
    bundleInfo'
