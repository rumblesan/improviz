{-# LANGUAGE OverloadedStrings #-}

module Configuration.Textures where

import           Data.Yaml                      ( (.:)
                                                , FromJSON(..)
                                                )
import qualified Data.Yaml                     as Y

data TextureConfig = TextureConfig
  { textureName :: String
  , textureFile :: FilePath
  }
  deriving (Eq, Show)

instance FromJSON TextureConfig where
  parseJSON (Y.Object v) = TextureConfig <$> v .: "name" <*> v .: "file"
  parseJSON _            = fail "Expected Object for Config value"

newtype TextureFolderConfig = TextureFolderConfig
  { textures :: [TextureConfig]
  } deriving (Eq, Show)

instance FromJSON TextureFolderConfig where
  parseJSON (Y.Object v) = TextureFolderConfig <$> v .: "textures"
  parseJSON _            = fail "Expected Object for Config value"
