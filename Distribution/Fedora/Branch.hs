{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Distribution.Fedora.Branch
  ( readBranch
  , readBranch'
  , Branch(..)
  , branchDestTag
  , newerBranch
  , releaseBranch
  , getFedoraBranches
  )
where

import Control.Applicative (
                             (<|>)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
                           , (<$>)
#endif
                           )

import Data.Char (isDigit)
import qualified Data.Text as T

import Distribution.Fedora (getFedoraReleaseIds)

data Branch = Fedora Int | Master
  deriving (Eq, Ord)

readBranch :: String -> Maybe Branch
readBranch bs = readBranch' bs <|> error' ("Unknown branch " ++ bs)

readBranch' :: String -> Maybe Branch
readBranch' "master" = Just Master
readBranch' ('f':ns) | all isDigit ns && ((read ns :: Int) `elem` [30,31]) = Just $ Fedora (read ns)
readBranch' _ = Nothing

instance Show Branch where
  show Master = "master"
  show (Fedora n) = "f" ++ show n
--  show (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n

branchDestTag :: Branch -> String
branchDestTag Master = "rawhide"
branchDestTag (Fedora n) = show (Fedora n) ++ "-updates-candidate"

--getLatestBranch :: IO Branch

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

releaseBranch :: T.Text -> Branch
releaseBranch "fedora-rawhide" = Master
releaseBranch rel | "fedora-" `T.isPrefixOf` rel =
                      let (_,ver) = T.breakOnEnd "-" rel in
                        Fedora $ read . show $ ver
                  | otherwise = error' $ "Unsupport release: " ++ T.unpack rel

getFedoraBranches :: IO [Branch]
getFedoraBranches = map releaseBranch <$> getFedoraReleaseIds

-- from simple-cmd
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif
