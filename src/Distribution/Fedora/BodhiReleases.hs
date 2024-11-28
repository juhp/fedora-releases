{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}

module Distribution.Fedora.BodhiReleases (
  getBodhiReleases,
  getBodhiProductReleases,
  getBodhiFedoraReleases,
  getBodhiEPELReleases,
  lookupKey
  )
where

import Control.Exception.Extra (retry)
import Data.Aeson (Object)
import Fedora.Bodhi (bodhiReleases, makeKey)
import System.Cached.JSON

-- FIXME softer/warning on failure?
-- | Get Releases from Fedora Bodhi API (excluding archived)
getBodhiReleases :: IO [Object]
getBodhiReleases =
  retry 2 $
  getCachedJSONQuery "fedora" "bodhi-releases.json"
  (bodhiReleases (makeKey "exclude_archived" "True")) 450

-- | Get Releases from Bodhi API filtered by id_prefix
getBodhiProductReleases :: String -> IO [Object]
getBodhiProductReleases name =
  reverse . filter (\r -> lookupKey "id_prefix" r == Just name) <$> getBodhiReleases

-- | Get FEDORA Releases from Bodhi API
getBodhiFedoraReleases :: IO [Object]
getBodhiFedoraReleases =
  getBodhiProductReleases "FEDORA"

-- | Get EPEL Releases from Bodhi API
getBodhiEPELReleases :: IO [Object]
getBodhiEPELReleases =
  getBodhiProductReleases "FEDORA-EPEL"
