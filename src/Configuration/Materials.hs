{-# LANGUAGE OverloadedStrings #-}

module Configuration.Materials where

import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                )
import qualified Data.Yaml                     as Y

data MaterialConfig = MaterialConfig
  { materialName :: String
  , materialFile :: FilePath
  } deriving (Eq, Show)

instance FromJSON MaterialConfig where
  parseJSON (Y.Object v) = MaterialConfig <$> v .: "name" <*> v .: "file"
  parseJSON _            = fail "Expected Object for Config value"

newtype MaterialFolderConfig = MaterialFolderConfig
  { materials :: [MaterialConfig]
  } deriving (Eq, Show)

instance FromJSON MaterialFolderConfig where
  parseJSON (Y.Object v) = MaterialFolderConfig <$> v .: "materials"
  parseJSON _            = fail "Expected Object for Config value"
