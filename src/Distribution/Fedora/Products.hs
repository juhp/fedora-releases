{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}

module Distribution.Fedora.Products
  ( Release(..)
  , parseReleases
  )
where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Control.Monad      (mzero)
import           Data.Aeson(eitherDecode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.=), object)
#if !MIN_VERSION_base(4,13,0)
import           Data.Monoid((<>))
#endif
import           Data.Text (Text)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif

data Release = Release {
    releaseProductVersionId :: Text,
    releaseReleases :: [Text],
    releaseAllowedPushTargets :: [[Maybe Value]],
    releaseActive :: Bool,
    releaseName :: Text,
    releaseVersion :: Text,
    releaseShort :: Text,
    releaseProduct :: Text
  } deriving (Show,Eq)

instance Ord Release where
  compare r1 r2 =
    compare (releaseProductVersionId r1) (releaseProductVersionId r2)

instance FromJSON Release where
  parseJSON (Object v) = Release <$> v .:  "product_version_id" <*> v .:  "releases" <*> v .:  "allowed_push_targets" <*> v .:  "active" <*> v .:  "name" <*> v .:  "version" <*> v .:  "short" <*> v .:  "product"
  parseJSON _          = mzero

instance ToJSON Release where
  toJSON     Release {..} = object ["product_version_id" .= releaseProductVersionId, "releases" .= releaseReleases, "allowed_push_targets" .= releaseAllowedPushTargets, "active" .= releaseActive, "name" .= releaseName, "version" .= releaseVersion, "short" .= releaseShort, "product" .= releaseProduct]
  toEncoding Release {..} = pairs  ("product_version_id" .= releaseProductVersionId<>"releases" .= releaseReleases<>"allowed_push_targets" .= releaseAllowedPushTargets<>"active" .= releaseActive<>"name" .= releaseName<>"version" .= releaseVersion<>"short" .= releaseShort<>"product" .= releaseProduct)

parseReleases :: FilePath -> IO [Release]
parseReleases filename = do
    input <- BL.readFile filename
    case eitherDecode input of
      Left  err -> error $ "Invalid JSON file: " ++ filename ++ "\n" ++ err
      Right r   -> return r
