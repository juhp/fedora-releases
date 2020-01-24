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
   distContainer,
   distRepo,
   distUpdates,
   distOverride,
   kojicmd,
   mockConfig,
   releaseVersion,
   rawhide,
   rawhideRelease,
   rpkg,
   rpmDistTag) where

import Data.Version
import Text.Read
import Text.ParserCombinators.ReadP (char, eof, string)

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

-- | Current maintained distribution releases.
dists :: [Dist]
dists = [rawhide, Fedora 31, Fedora 30, EPEL 8, EPEL 7]

-- | The Fedora release number corresponding to current Rawhide
rawhideRelease :: Int
rawhideRelease = 32

-- | The Fedora release corresponding to Rawhide
rawhide :: Dist
rawhide = Fedora rawhideRelease

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
distOverride d = d `notElem` [rawhide, Fedora 32 , EPEL 9]

-- | OS release major version for `Dist`
releaseVersion :: Dist -> String
releaseVersion (Fedora n) | n >= rawhideRelease = "rawhide"
releaseVersion (Fedora n) = show n
releaseVersion (EPEL n) = show n
releaseVersion (RHEL n) = show n

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
mockConfig :: Dist -> String -> String
mockConfig dist arch =
  let prefix =
        case dist of
          Fedora _ -> "fedora"
          _ -> distRepo dist
  in
  prefix ++ "-" ++ releaseVersion dist ++ "-" ++ arch

-- | Map `Dist` to a container image
distContainer :: Dist -> String
distContainer (Fedora n) = "fedora:" ++ show n
distContainer (EPEL n) = "centos:" ++ show n
distContainer (RHEL n) = "ubi" ++ show n ++ "/ubi"
