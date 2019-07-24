{-# LANGUAGE OverloadedStrings #-}

module Configuration.Geometries where

import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                )
import qualified Data.Yaml                     as Y

data GeometryConfig = GeometryConfig
  { geometryName :: String
  , geometryFile :: FilePath
  } deriving (Eq, Show)

instance FromJSON GeometryConfig where
  parseJSON (Y.Object v) = GeometryConfig <$> v .: "name" <*> v .: "file"
  parseJSON _            = fail "Expected Object for Config value"

newtype GeometryFolderConfig = GeometryFolderConfig
  { geometries :: [GeometryConfig]
  } deriving (Eq, Show)

instance FromJSON GeometryFolderConfig where
  parseJSON (Y.Object v) = GeometryFolderConfig <$> v .: "geometries"
  parseJSON _            = fail "Expected Object for Config value"
