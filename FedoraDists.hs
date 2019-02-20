-- |
-- Module      :  FedoraDists
-- Copyright   :  (C) 2014-2018  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Fedora dist metadata

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module FedoraDists
  (Dist(..),
   dists,
   distBranch,
   distRepo,
   distTag,
   distTarget,
   distUpdates,
   distOverride,
   hackageRelease,
   kojicmd,
   mockConfig,
   releaseVersion,
   rawhide,
   rawhideRelease,
   rpkg,
   rpmDistTag) where

import Data.Version
import Data.Maybe (fromMaybe)
import Text.Read

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (*>))
import Data.Traversable (traverse)
#endif

-- | The `Dist` datatype specifies the target OS and version.
-- (roughly corresponds to a git branch)
data Dist = Fedora Int -- ^ Fedora release
          | EPEL Int -- ^ EPEL release
          | RHEL Version -- ^ RHEL version
  deriving (Eq)

instance Show Dist where
  show (Fedora n) = "f" ++ show n
  show (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n
  show (RHEL v) = "rhel-" ++ show v

-- | Read from eg "f29", "epel7"
instance Read Dist where
  readPrec = choice [pFedora, pEPEL, pRHEL] where
    pChar c = do
      c' <- get
      if c == c'
        then return c
        else pfail
    pFedora = Fedora <$> (pChar 'f' *> readPrec)
    pEPEL = EPEL <$> (traverse pChar "epel" *> readPrec)
    pRHEL = RHEL <$> (traverse pChar "rhel-" *> readPrec)

  readListPrec = readListPrecDefault

-- | Current maintained distribution releases.
dists :: [Dist]
dists = [rawhide, Fedora 30, Fedora 29, Fedora 28, EPEL 7, EPEL 6]

-- | The Fedora release number corresponding to current Rawhide
rawhideRelease :: Int
rawhideRelease = 31

-- | The Fedora release corresponding to Rawhide
rawhide :: Dist
rawhide = Fedora rawhideRelease

-- | Used for Koji sidetag when needed.
sidetag :: Dist -> Maybe String
--sidetag (Fedora n) | n == rawhideRelease = Just "ghc"
sidetag _ = Nothing

-- | The Fedora release being tracked in Hackage Distro data (`rawhideRelease` - 1)
hackageRelease :: Dist
hackageRelease = Fedora (rawhideRelease - 1)

-- | Maps `Dist` to package distgit branch
distBranch :: Dist -> String
distBranch (Fedora n) | n >= rawhideRelease = "master"
distBranch d = show d

-- | Map `Dist` to DNF/YUM repo name
distRepo :: Dist -> String
distRepo (Fedora n) | n >= rawhideRelease = "rawhide"
                    | otherwise = "fedora"
distRepo (EPEL _) = "epel"
distRepo (RHEL _) = "rhel"

-- | Map `Dist` to Maybe the DNF/YUM updates repo name
distUpdates :: Dist -> Maybe String
distUpdates (Fedora n) | n >= rawhideRelease = Nothing
distUpdates (Fedora _) = Just "updates"
distUpdates _ = Nothing

-- | Whether dist has overrides in Bodhi
distOverride :: Dist -> Bool
distOverride d = d `notElem` [rawhide, Fedora 30 , EPEL 8]

distTag :: Dist -> String
distTag d = show d ++ "-" ++ fromMaybe "build" (sidetag d)

distTarget  :: Dist -> String
distTarget d = show d ++ "-" ++ fromMaybe "" (sidetag d)

releaseVersion :: Dist -> String
releaseVersion (Fedora n) | n >= rawhideRelease = "rawhide"
releaseVersion (Fedora n) = show n
releaseVersion (EPEL n) = show n
releaseVersion (RHEL n) = show n

rpmDistTag :: Dist -> String
rpmDistTag (Fedora n) = ".fc" ++ show n
rpmDistTag (RHEL v) = ".el" ++ (show . head . versionBranch) v
rpmDistTag d = '.' : show d

kojicmd :: Dist -> String
kojicmd (RHEL _) = "brew"
kojicmd _ =  "koji"

rpkg :: Dist -> String
rpkg (RHEL _) = "rhpkg"
rpkg _ = "fedpkg"

mockConfig :: Dist -> String -> String
mockConfig dist arch =
  distRepo dist ++ "-" ++ releaseVersion dist ++ "-" ++ arch

distContainer :: Dist -> String
distContainer (Fedora n) = "fedora:" ++ show n
distContainer (EPEL n) = "centos:" ++ show n
distContainer (RHEL n) = "rhel" ++ show n
