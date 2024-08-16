{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}

module Distribution.Fedora.BodhiReleases
  (getBodhiReleases)
where

import Control.Exception.Extra (retry)
import Data.Aeson (Object)
import Fedora.Bodhi (bodhiReleases, makeKey)
import System.Cached.JSON

-- FIXME softer/warning on failure?
getBodhiReleases :: IO [Object]
getBodhiReleases =
  retry 2 $
  getCachedJSONQuery "fedora" "bodhi-releases.json"
  (bodhiReleases (makeKey "exclude_archived" "True")) 450
