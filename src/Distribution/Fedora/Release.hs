{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}

-- |
-- Module      :  Distribution.Fedora.Release
-- Copyright   :  (C) 2020-2022,2024  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: Fedora Release type and functions

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Distribution.Fedora.Release
  ( Release(..),
    getActiveReleases,
    getFedoraReleases,
    getEPELReleases
  )
where

import Control.Monad (guard)
import Data.Aeson(Object)
import Data.Maybe (mapMaybe)
import Distribution.Fedora.BodhiReleases

-- | Fedora Release data
data Release = Release {
    releaseName :: String,
    releaseVersion :: String,
    releaseIdPrefix :: String,
    releaseBranch :: String,
    releaseComposed :: Bool,
    releaseCandidateTag :: String,
    releaseDistTag :: String,
    releaseSettingStatus :: Maybe String,
    releaseState :: String,
    releaseAutomaticUpdates :: Bool,
    releaseTestingRepo :: Maybe String,
    releaseTestingTag :: String
  } deriving (Show,Eq)

readRelease :: Object -> Maybe Release
readRelease obj = do
  name <- lookupKey "name" obj
  ver <- lookupKey "version" obj
  idPref <- lookupKey "id_prefix" obj
  guard (idPref `notElem` ["FEDORA-CONTAINER","FEDORA-FLATPAK"])
  br <- lookupKey "branch" obj
  composed <- lookupKey "composed_by_bodhi" obj
  candidate <- lookupKey "candidate_tag" obj
  disttag <- lookupKey "dist_tag" obj
  let setting = lookupKey "setting_status" obj
  state <- lookupKey "state" obj
  automatic <- lookupKey "create_automatic_updates" obj
  let testrepo = lookupKey "testing_repository" obj
  testtag <- lookupKey "testing_tag" obj
  return $ Release name ver idPref br composed candidate disttag setting state automatic testrepo testtag

instance Ord Release where
  compare r1 r2 =
    compare (releaseName r1) (releaseName r2)

-- FIXME remove containers and flatpaks
-- | Get list of all current Fedora Project releases (from Bodhi)
getActiveReleases :: IO [Release]
getActiveReleases =
  mapMaybe readRelease <$> getBodhiReleases

getProductReleases :: String -> IO [Release]
getProductReleases name =
  mapMaybe readRelease <$> getBodhiProductReleases name

-- | Get list of current Fedora Linux releases
getFedoraReleases :: IO [Release]
getFedoraReleases =
  getProductReleases "FEDORA"

-- | Get list of current Fedora EPEL releases
getEPELReleases :: IO [Release]
getEPELReleases =
  getProductReleases "FEDORA-EPEL"
