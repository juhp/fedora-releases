{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}

module Distribution.Fedora.ReadProducts
  (getProductsFile)
where

import Control.Monad
import Data.Aeson(encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Fedora.PDC (makeKey, pdcProductVersions)
import System.Directory
import System.FilePath ((</>))

getProductsFile :: IO FilePath
getProductsFile = do
  home <- getHomeDirectory
  let dir = home </> ".fedora"
  dirExists <- doesDirectoryExist dir
  unless dirExists $ createDirectory dir
  let file = dir </> "product-versions.json"
  recent <- do
    have <- doesFileExist file
    if have then do
      ts <- getModificationTime file
      t <- getCurrentTime
      -- about 5.5 hours
      return $ diffUTCTime t ts < 20000
      else return False
  unless recent $
    pdcProductVersions "pdc.fedoraproject.org" (makeKey "active" "true") >>=
    BL.writeFile file . encode
  return file
