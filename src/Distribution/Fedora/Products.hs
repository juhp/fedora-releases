{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}

module Distribution.Fedora.Products
  (getProducts)
where

import Data.Aeson (Object)
import Fedora.PDC (makeKey, pdcProductVersions)
import System.Cached.JSON

getProducts :: IO [Object]
getProducts =
  getCachedJSONQuery "fedora" "product-versions.json"
  (pdcProductVersions "pdc.fedoraproject.org" (makeKey "active" "true")) 300
