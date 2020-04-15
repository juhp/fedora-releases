{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

-- |
-- Module      :  Distribution.Fedora.Branch
-- Copyright   :  (C) 2020  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
--
-- Explanation: Fedora Branch type and functions

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Distribution.Fedora.Branch
  ( Branch(..)
  , readBranch
  , readBranch'
  , eitherBranch
  , readActiveBranch
  , readActiveBranch'
  , eitherActiveBranch
  , branchDestTag
  , newerBranch
  , getFedoraBranches
  , getFedoraBranched
  )
where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative (
                           (<$>)
                           )
#endif

import Data.Char (isDigit)
import qualified Data.Text as T

import Distribution.Fedora (getFedoraReleaseIds)

-- | Branch datatype
--
-- Currently only supports master and Fedora branches.
data Branch = Fedora Int | Master
  deriving (Eq, Ord)

-- | Read a Fedora Branch name
eitherBranch :: String -> Either String Branch
eitherBranch "master" = Right Master
eitherBranch ('f':ns) | all isDigit ns = let br = Fedora (read ns) in Right br
eitherBranch _ = Left "unknown Fedora branch"

-- | Read a Fedora Branch name
readBranch :: String -> Maybe Branch
readBranch bs =
  case eitherBranch bs of
    Left _ -> Nothing
    Right br -> Just br

-- | Unsafely read a Fedora Branch name: errors for unknown branches
readBranch' :: String -> Branch
readBranch' bs =
  case eitherBranch bs of
    Left e -> error' e
    Right br -> br

-- | Read a Branch name (one of the list of active branches)
--
-- Provides error strings for inactive or unknown branches.
eitherActiveBranch :: [Branch] -> String -> Either String Branch
eitherActiveBranch active bs =
  case readBranch bs of
    Just br -> if br `elem` active
               then Right br
               else Left ("inactive Fedora branch: " ++ bs)
    Nothing -> Left $ "unknown Fedora branch: " ++ bs

-- | Read a Branch name (one of the list of active branches)
--
-- Similar to eitherActiceBranch but ignores any error string
readActiveBranch :: [Branch] -> String -> Maybe Branch
readActiveBranch active cs =
  case eitherActiveBranch active cs of
    Left _ -> Nothing
    Right br -> Just br

-- | Read a Branch name (one of the list of active branches)
--
-- Like readActiveBranch, but errors for inactive or unknown branches.
readActiveBranch' :: [Branch] -> String -> Branch
readActiveBranch' active cs =
  case eitherActiveBranch active cs of
    Left e -> error' e
    Right br -> br

instance Show Branch where
  show Master = "master"
  show (Fedora n) = "f" ++ show n
--  show (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n

-- | Map Branch to Koji destination tag
branchDestTag :: Branch -> String
branchDestTag Master = "rawhide"
branchDestTag (Fedora n) = show (Fedora n) ++ "-updates-candidate"

--getLatestBranch :: IO Branch

-- | Returns newer branch than given one from supplied active branches.
newerBranch :: [Branch] -> Branch -> Branch
newerBranch _ Master = Master
newerBranch branches (Fedora n) =
  if Fedora n `elem` branches
  then if Fedora (n+1) `elem` branches
       then Fedora (n+1)
       else Master
  else error' $ "Unsupported branch: " ++ show (Fedora n)

--olderBranch :: Branch -> Branch
--olderBranch Master = latestBranch
--olderBranch (Fedora n) = Fedora (n-1)

-- | Returns list of active Fedora branches, including master
getFedoraBranches :: IO [Branch]
getFedoraBranches = map releaseBranch <$> getFedoraReleaseIds
  where
    -- | Maps release-id to Branch
    releaseBranch :: T.Text -> Branch
    releaseBranch "fedora-rawhide" = Master
    releaseBranch rel | "fedora-" `T.isPrefixOf` rel =
                          let (_,ver) = T.breakOnEnd "-" rel in
                            Fedora $ read . T.unpack $ ver
                      | otherwise = error' $ "Unsupport release: " ++ T.unpack rel

-- | Returns list of active Fedora branches, excluding master
getFedoraBranched :: IO [Branch]
getFedoraBranched = filter (/= Master) <$> getFedoraBranches

-- from simple-cmd
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif
