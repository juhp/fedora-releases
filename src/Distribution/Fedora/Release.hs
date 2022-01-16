{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}

module Distribution.Fedora.Release
  ( Release(releaseProductVersionId,releaseVersion,releaseProduct),
    readRelease
  )
where

import Data.Aeson(Object)
import Data.Text (Text)
import Fedora.PDC (lookupKey)

data Release = Release {
    releaseProductVersionId :: Text,
--    releaseReleases :: [Text],
--    releaseAllowedPushTargets :: [[Maybe Value]],
--    releaseActive :: Bool,
--    releaseName :: Text,
    releaseVersion :: Text,
--    releaseShort :: Text,
    releaseProduct :: Text
  } deriving (Show,Eq)

instance Ord Release where
  compare r1 r2 =
    compare (releaseProductVersionId r1) (releaseProductVersionId r2)

readRelease :: Object -> Maybe Release
readRelease obj = do
  pvid <- lookupKey "product_version_id" obj
  ver <- lookupKey "version" obj
  prod <- lookupKey "product" obj
  return $ Release pvid ver prod
