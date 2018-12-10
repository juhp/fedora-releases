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
#endif

data Dist = Fedora Int | EPEL Int | RHEL Version
  deriving (Eq)

instance Show Dist where
  show (Fedora n) = "f" ++ show n
  show (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n
  show (RHEL v) = "rhel-" ++ show v

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

dists :: [Dist]
dists = [rawhide, Fedora 29, Fedora 28, EPEL 7]

rawhideRelease :: Int
rawhideRelease = 30

rawhide :: Dist
rawhide = Fedora rawhideRelease

sidetag :: Dist -> Maybe String
--sidetag (Fedora n) | n == rawhideRelease = Just "ghc"
sidetag _ = Nothing

hackageRelease :: Dist
hackageRelease = Fedora (rawhideRelease - 1)

distBranch :: Dist -> String
distBranch (Fedora n) | n >= rawhideRelease = "master"
distBranch d = show d

distRepo :: Dist -> String
distRepo (Fedora n) | n >= rawhideRelease = "rawhide"
                    | otherwise = "fedora"
distRepo (EPEL _) = "epel"
distRepo (RHEL _) = "rhel"

distUpdates :: Dist -> Maybe String
distUpdates (Fedora n) | n >= rawhideRelease = Nothing
distUpdates (Fedora _) = Just "updates"
distUpdates _ = Nothing

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
