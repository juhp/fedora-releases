{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}

module Distribution.Fedora.Products
  ( Release(..)
  , parseReleases
  )
where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Control.Monad      (mzero)
import           Data.Aeson(eitherDecode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid((<>))
import           Data.Text (Text)
import qualified GHC.Generics

data Release = Release {
    releaseProductVersionId :: Text,
    releaseReleases :: [Text],
    releaseAllowedPushTargets :: [[Maybe Value]],
    releaseActive :: Bool,
    releaseName :: Text,
    releaseVersion :: Text,
    releaseShort :: Text,
    releaseProduct :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)

instance Ord Release where
  compare r1 r2 =
    compare (releaseProductVersionId r1) (releaseProductVersionId r2)

instance FromJSON Release where
  parseJSON (Object v) = Release <$> v .:  "product_version_id" <*> v .:  "releases" <*> v .:  "allowed_push_targets" <*> v .:  "active" <*> v .:  "name" <*> v .:  "version" <*> v .:  "short" <*> v .:  "product"
  parseJSON _          = mzero


instance ToJSON Release where
  toJSON     Release {..} = object ["product_version_id" .= releaseProductVersionId, "releases" .= releaseReleases, "allowed_push_targets" .= releaseAllowedPushTargets, "active" .= releaseActive, "name" .= releaseName, "version" .= releaseVersion, "short" .= releaseShort, "product" .= releaseProduct]
  toEncoding Release {..} = pairs  ("product_version_id" .= releaseProductVersionId<>"releases" .= releaseReleases<>"allowed_push_targets" .= releaseAllowedPushTargets<>"active" .= releaseActive<>"name" .= releaseName<>"version" .= releaseVersion<>"short" .= releaseShort<>"product" .= releaseProduct)


data ProductReleases = ProductReleases {
    productsNext :: Maybe Value,
    productsResults :: [Release],
    productsCount :: Int,
    productsPrevious :: Maybe Value
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ProductReleases where
  parseJSON (Object v) = ProductReleases <$> v .:? "next" <*> v .:  "results" <*> v .:  "count" <*> v .:? "previous"
  parseJSON _          = mzero


instance ToJSON ProductReleases where
  toJSON     ProductReleases {..} = object ["next" .= productsNext, "results" .= productsResults, "count" .= productsCount, "previous" .= productsPrevious]
  toEncoding ProductReleases {..} = pairs  ("next" .= productsNext<>"results" .= productsResults<>"count" .= productsCount<>"previous" .= productsPrevious)


parseReleases :: FilePath -> IO [Release]
parseReleases filename = do
    input <- BSL.readFile filename
    case eitherDecode input of
      Left  err -> error $ "Invalid JSON file: " ++ filename ++ "\n" ++ err
      Right r   -> return $ productsResults r
