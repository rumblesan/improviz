{-# LANGUAGE OverloadedStrings #-}

module Configuration.Materials where

import Data.Maybe (fromMaybe)
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:), (.:?)
                                                )
import qualified Data.Yaml                     as Y

data MaterialConfig = MaterialConfig
  { materialName :: String
  , materialFile :: FilePath
  } deriving (Eq, Show)

instance FromJSON MaterialConfig where
  parseJSON (Y.Object v) = MaterialConfig <$> v .: "name" <*> v .: "file"
  parseJSON _            = fail "Expected Object for Config value"

data MaterialFolderConfig = MaterialFolderConfig
  { materialsConfig :: [MaterialConfig]
  , folderVarDefaults :: [(String, Float)]
  } deriving (Eq, Show)

instance FromJSON MaterialFolderConfig where
  parseJSON (Y.Object v) = do
    materials <- v .: "materials"
    defaultsArray <- v .:? "defaults"
    varDefaults <- mapM (\o -> (,) <$> o .: "name" <*> o .: "value") (fromMaybe [] defaultsArray)
    return $ MaterialFolderConfig materials varDefaults
  parseJSON _            = fail "Expected Object for Config value"
