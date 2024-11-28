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
    getReleases,
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
    releaseBranch :: String
  } deriving (Show,Eq)

instance Ord Release where
  compare r1 r2 =
    compare (releaseName r1) (releaseName r2)

readRelease :: Object -> Maybe Release
readRelease obj = do
  name <- lookupKey "name" obj
  ver <- lookupKey "version" obj
  idPref <- lookupKey "id_prefix" obj
  guard (idPref `notElem` ["FEDORA-CONTAINER","FEDORA-FLATPAK"])
  br <- lookupKey "branch" obj
  return $ Release name ver idPref br

-- FIXME remove containers and flatpaks
-- | Get list of all current Fedora Project releases (from Bodhi)
getReleases :: IO [Release]
getReleases =
  reverse . mapMaybe readRelease <$> getBodhiReleases

getProductReleases :: String -> IO [Release]
getProductReleases name =
  filter (\p -> releaseIdPrefix p == name) <$> getReleases

-- | Get list of current Fedora Linux releases
getFedoraReleases :: IO [Release]
getFedoraReleases =
  getProductReleases "FEDORA"

-- | Get list of current Fedora EPEL releases
getEPELReleases :: IO [Release]
getEPELReleases =
  getProductReleases "FEDORA-EPEL"
