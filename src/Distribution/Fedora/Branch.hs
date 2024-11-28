-- |
-- Module      :  Distribution.Fedora.Branch
-- Copyright   :  (C) 2020-2022,2024  Jens Petersen
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
  , eitherBranch
  , readActiveBranch
  , eitherActiveBranch
  , newerBranch
  , getActiveBranches
  , getActiveBranched
  , getLatestFedoraBranch
  , branchDestTag
  , branchDistTag
  , branchRelease
  , branchTarget
  , partitionBranches
  )
where

import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.List (delete, elemIndex, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing, Down(Down))
import Data.Tuple (swap)
import Safe (headDef)

import Distribution.Fedora.Release

-- | Branch datatype
--
-- Branch can be rawhide, or a fedora or epel branch
data Branch = EPEL !Int | EPELNext !Int | Fedora !Int | Rawhide
  deriving (Eq)

instance Ord Branch where
  compare Rawhide Rawhide = EQ
  compare (Fedora m) (Fedora n) = compare m n
  compare (EPELNext m) (EPELNext n) = compare m n
  compare (EPEL m) (EPEL n) = compare m n
  compare Rawhide _ = GT
  compare _ Rawhide = LT
  compare (Fedora _) _ = GT
  compare _ (Fedora _) = LT
  compare (EPEL m) (EPELNext n) = if m == n then LT else compare m n
  compare (EPELNext m) (EPEL n) = if m == n then GT else compare m n

-- | Read a Fedora Branch name, otherwise return branch string
eitherBranch :: String -> Either String Branch
eitherBranch "rawhide" = Right Rawhide
eitherBranch ('f':ns) | all isDigit ns = let br = Fedora (read ns) in Right br
-- FIXME add proper parsing:
eitherBranch "epel8-next" = Right $ EPELNext 8
eitherBranch "epel9-next" = Right $ EPELNext 9
eitherBranch ('e':'p':'e':'l':n) | all isDigit n = let br = EPEL (read n) in Right br
eitherBranch ('e':'l':n) | all isDigit n = let br = EPEL (read n) in Right br
eitherBranch cs = Left cs

-- -- | Read a Fedora Branch name, otherwise return an error message
-- eitherBranch' :: String -> Either String Branch
-- eitherBranch' cs = case eitherBranch cs of
--   Right br -> Right br
--   Left xs -> Left $ xs ++ " is not a known Fedora/EPEL branch"

-- | Read a Fedora Branch name
readBranch :: String -> Maybe Branch
readBranch bs =
  case eitherBranch bs of
    Left _ -> Nothing
    Right br -> Just br

-- | Read a Branch name (one of the list of active branches)
--
-- Provides error strings for inactive or unknown branches.
eitherActiveBranch :: [Branch] -> String -> Either String Branch
eitherActiveBranch active bs =
  case eitherBranch bs of
    Left e -> Left e
    Right br -> if br `elem` active
                then Right br
                else Left bs

-- | Read a Branch name (one of the list of active branches)
--
-- Similar to eitherActiveBranch but ignores any error string
readActiveBranch :: [Branch] -> String -> Maybe Branch
readActiveBranch active cs =
  case eitherActiveBranch active cs of
    Left _ -> Nothing
    Right br -> Just br

instance Show Branch where
  show Rawhide = "rawhide"
  show (Fedora n) = "f" ++ show n
  show (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n
  show (EPELNext n) = "epel" ++ show n ++ "-next"

-- | Get Release associated with release Branch
branchRelease :: Branch -> IO Release
branchRelease br = do
  rels <- getActiveReleases
  case releaseFilter releaseBranch (== show br) rels of
    [] -> error' $ "release not found for branch " ++ show br
    [rel] -> return rel
    rs -> error' $ "impossible happened: multiple releases for " ++ show br ++ ":\n" ++ unwords (map releaseBranch rs)

-- | Map Branch to Koji destination tag
branchDestTag :: Branch -> String
branchDestTag Rawhide = "rawhide"
branchDestTag (Fedora n) = show (Fedora n) ++ "-updates-candidate"
branchDestTag (EPEL n) = show (EPEL n) ++ "-testing-candidate"
branchDestTag (EPELNext n) = show (EPELNext n) ++ "-testing-candidate"

-- | Get %dist tag for branch
branchDistTag :: Branch -> IO String
branchDistTag Rawhide = do
  n <- releaseVersion <$> branchRelease Rawhide
  branchDistTag (Fedora (read n))
branchDistTag (Fedora n) = return $ ".fc" ++ show n
branchDistTag (EPEL n) = return $ ".el" ++ show n
branchDistTag (EPELNext n) = return $ ".el" ++ show n ++ ".next"

-- | Default build target associated with a branch
branchTarget :: Branch -> String
branchTarget (Fedora n) = show (Fedora n)
branchTarget (EPEL n) = show (EPEL n)
branchTarget (EPELNext n) = show (EPELNext n)
branchTarget Rawhide = "rawhide"

-- | Returns newer branch than given one from supplied active branches.
--
-- Branches should be in descending order, eg from getFedoraBranches
newerBranch :: Branch -> [Branch] -> Maybe Branch
newerBranch Rawhide _ = Nothing
newerBranch br branches =
  case elemIndex br branches of
    Just i | i > 0 -> Just $ branches !! (i - 1)
    _ -> Nothing


--olderBranch :: Branch -> Branch
--olderBranch Rawhide = latestBranch
--olderBranch (Fedora n) = Fedora (n-1)

-- | Returns descending list of active Fedora branches, including rawhide and EPEL
getActiveBranches :: IO [Branch]
getActiveBranches =
  reverseSort . mapMaybe (readBranch . releaseBranch) <$> getActiveReleases

-- | Returns list of active Fedora branches, excluding rawhide
getActiveBranched :: IO [Branch]
getActiveBranched = delete Rawhide <$> getActiveBranches

-- from simple-cmd
error' :: String -> a
error' = errorWithoutStackTrace

-- | separate fedora branches from rest of args
partitionBranches :: [String] -> ([Branch],[String])
partitionBranches args =
  swap . partitionEithers $ map eitherBranch args

-- | get newest Fedora branched Release
getLatestFedoraBranch :: IO Branch
getLatestFedoraBranch =
  headDef (error' "no active branched!") <$> getActiveBranched

releaseFilter :: (Release -> a) -> (a -> Bool) -> [Release] -> [Release]
releaseFilter f p = filter (p . f)

reverseSort :: Ord a => [a] -> [a]
reverseSort = sortBy (comparing Down)
