{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}

module Distribution.Fedora.Products where

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Control.Monad      (mzero)
import           Data.Aeson(eitherDecode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid((<>))
import           Data.Text (Text)
import qualified GHC.Generics

data Product = Product {
    productAllowedPushTargets :: [[Maybe Value]],
    productActive :: Bool,
    productName :: Text,
    productShort :: Text,
    productVersions :: [Text]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Product where
  parseJSON (Object v) = Product <$> v .:  "allowed_push_targets" <*> v .:  "active" <*> v .:  "name" <*> v .:  "short" <*> v .:  "product_versions"
  parseJSON _          = mzero


instance ToJSON Product where
  toJSON     Product {..} = object ["allowed_push_targets" .= productAllowedPushTargets, "active" .= productActive, "name" .= productName, "short" .= productShort, "product_versions" .= productVersions]
  toEncoding Product {..} = pairs  ("allowed_push_targets" .= productAllowedPushTargets<>"active" .= productActive<>"name" .= productName<>"short" .= productShort<>"product_versions" .= productVersions)


data Products = Products {
    productsNext :: Maybe Value,
    productsResults :: [Product],
    productsCount :: Int,
    productsPrevious :: Maybe Value
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Products where
  parseJSON (Object v) = Products <$> v .:? "next" <*> v .:  "results" <*> v .:  "count" <*> v .:? "previous"
  parseJSON _          = mzero


instance ToJSON Products where
  toJSON     Products {..} = object ["next" .= productsNext, "results" .= productsResults, "count" .= productsCount, "previous" .= productsPrevious]
  toEncoding Products {..} = pairs  ("next" .= productsNext<>"results" .= productsResults<>"count" .= productsCount<>"previous" .= productsPrevious)


parse :: FilePath -> IO Products
parse filename = do
    input <- BSL.readFile filename
    case eitherDecode input of
      Left  err -> error $ "Invalid JSON file: " ++ filename ++ "\n" ++ err
      Right r   -> return (r :: Products)
