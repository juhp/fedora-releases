{-# LANGUAGE CPP #-}

-- |
-- Module      :  FedoraDists
-- Copyright   :  (C) 2014-2020  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: Fedora dist metadata

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Distribution.Fedora
  (Dist(..),
   distBranch,
   distContainer,
   distRepo,
   distUpdates,
   distOverride,
   --getReleases,
   getReleaseIds,
   --getFedoraReleases,
   getFedoraReleaseIds,
   getFedoraDists,
   --getEPELReleases,
   getEPELReleaseIds,
   getRawhideDist,
   getLatestFedoraDist,
   getLatestEPELDist,
   rawhideVersionId,
   kojicmd,
   mockConfig,
   distVersion,
   rpkg,
   rpmDistTag) where

import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Version
import System.Directory
import System.FilePath ((</>))
import System.Process (rawSystem)
import Text.Read
import Text.ParserCombinators.ReadP (char, eof, string)

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (*>))
import Data.Traversable (traverse)
#endif

import Distribution.Fedora.Products

-- | The `Dist` datatype specifies the target OS and version.
-- (roughly corresponds to a git branch)
data Dist = RHEL Version -- ^ RHEL version
          | EPEL Int -- ^ EPEL release
          | Fedora Int -- ^ Fedora release
  deriving (Eq, Ord)

instance Show Dist where
  show (Fedora n) = "f" ++ show n
  show (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n
  show (RHEL v) = "rhel-" ++ showVersion v

-- | Read from eg "f29", "epel7"
instance Read Dist where
  readPrec = choice [pFedora, pEPEL, pRHEL] where
    pFedora = Fedora <$> (lift (char 'f') *> readPrec)
    pEPEL = EPEL <$> (lift (string "epel") *> readPrec)
    pRHEL = RHEL <$> lift (do
      v <- string "rhel-" >> parseVersion
      eof
      return v)

getProductsFile :: IO FilePath
getProductsFile = do
  home <- getHomeDirectory
  let file = home </> ".fedora/product-versions.json"
  recent <- do
    have <- doesFileExist file
    if have then do
      ts <- getModificationTime file
      t <- getCurrentTime
      -- about 5.5 hours
      return $ diffUTCTime t ts < 20000
    else return False
  unless recent $
    void $ rawSystem "curl" ["--silent", "--show-error", "-o", file, "https://pdc.fedoraproject.org/rest_api/v1/product-versions/?active=true"]
  return file

getReleases :: IO [Release]
getReleases = do
  file <- getProductsFile
  reverse <$> parseReleases file

getReleaseIds :: IO [Text]
getReleaseIds = map releaseProductVersionId <$> getReleases

getProductReleases :: Text -> IO [Release]
getProductReleases name =
  filter (\p -> releaseProduct p == name) <$> getReleases

-- getRelease :: Text -> IO (Maybe Release)
-- getRelease pv =
--   find (\p -> releaseProductVersionId p == pv) <$> getReleases

getFedoraReleases :: IO [Release]
getFedoraReleases =
  getProductReleases (T.pack "Fedora")

getFedoraReleaseIds :: IO [Text]
getFedoraReleaseIds =
  map releaseProductVersionId <$> getFedoraReleases

getEPELReleases :: IO [Release]
getEPELReleases =
  getProductReleases (T.pack "EPEL")

getEPELReleaseIds :: IO [Text]
getEPELReleaseIds =
  map releaseProductVersionId <$> getEPELReleases

rawhideVersionId :: Text
rawhideVersionId = T.pack "fedora-rawhide"

-- FIXME fails on rawhide
releaseMajorVersion :: Release -> Int
releaseMajorVersion = read . show . releaseVersion

releaseDist :: Release -> Dist
releaseDist = Fedora . releaseMajorVersion

releaseDists :: [Release] -> [Dist]
releaseDists rels =
  map mkDist rels
  where
    mkDist :: Release -> Dist
    mkDist r | releaseProductVersionId r == rawhideVersionId = newerDist latestbranch
             | otherwise = releaseDist r

    latestbranch = maximum $ filter (\p -> releaseProductVersionId p /= rawhideVersionId) rels

    newerDist = Fedora . (+ 1) . releaseMajorVersion

getFedoraDists :: IO [Dist]
getFedoraDists = releaseDists <$> getFedoraReleases

getRawhideDist :: IO Dist
getRawhideDist =
  head . releaseDists <$> getFedoraReleases

getLatestFedoraDist :: IO Dist
getLatestFedoraDist =
  releaseDist . maximum . filter (\p -> releaseProductVersionId p /= rawhideVersionId) <$> getFedoraReleases

getLatestEPELDist :: IO Dist
getLatestEPELDist =
  EPEL . releaseMajorVersion . maximum <$> getEPELReleases

-- activeRelease :: Text -> IO Bool
-- activeRelease pv = do
--   res <- filter (\p -> releaseProductVersionId p == pv) <$> getReleases
--   return $ not (null res)

-- | Maps `Dist` to package distgit branch
distBranch :: Dist -> Dist -> String
distBranch branch (Fedora n) | Fedora n > branch = "master"
distBranch _ d = show d

-- | Map `Dist` to DNF/YUM repo name
distRepo :: Dist -> Dist -> String
distRepo branched (Fedora n) | Fedora n > branched = "rawhide"
                             | otherwise = "fedora"
distRepo _ (EPEL _) = "epel"
distRepo _ (RHEL _) = "rhel"

-- | Map `Dist` to Maybe the DNF/YUM updates repo name
distUpdates :: Dist -> Dist -> Maybe String
distUpdates branched (Fedora n) | Fedora n > branched  = Nothing
distUpdates _ (Fedora _) = Just "updates"
distUpdates _ _ = Nothing

-- | Whether dist has overrides in Bodhi
distOverride :: Dist -> Dist -> Bool
distOverride branch (Fedora n) = Fedora n <= branch
distOverride _ (EPEL n) = n < 9
distOverride _ _ = False

-- | OS release major version for `Dist`
distVersion :: Dist -> Dist -> String
distVersion branch (Fedora n) | Fedora n > branch = "rawhide"
distVersion _ (Fedora n) = show n
distVersion _ (EPEL n) = show n
distVersion _ (RHEL n) = show n

-- | `Dist` tag (appended to rpm package release field)
rpmDistTag :: Dist -> String
rpmDistTag (Fedora n) = ".fc" ++ show n
rpmDistTag (RHEL v) = ".el" ++ (show . head . versionBranch) v
rpmDistTag d = '.' : show d

-- | Command line tool for `Dist` (eg "koji")
kojicmd :: Dist -> String
kojicmd (RHEL _) = "brew"
kojicmd _ =  "koji"

-- | rpkg command for `Dist` (eg "fedpkg")
rpkg :: Dist -> String
rpkg (RHEL _) = "rhpkg"
rpkg _ = "fedpkg"

-- | Mock configuration for `Dist` and arch
mockConfig :: Dist -> Dist -> String -> String
mockConfig branch dist arch =
  let prefix =
        case dist of
          Fedora _ -> "fedora"
          _ -> distRepo branch dist
  in
  prefix ++ "-" ++ distVersion branch dist ++ "-" ++ arch

-- | Map `Dist` to a container image
distContainer :: Dist -> String
distContainer (Fedora n) = "fedora:" ++ show n
distContainer (EPEL n) = "centos:" ++ show n
distContainer (RHEL n) = "ubi" ++ show n ++ "/ubi"
