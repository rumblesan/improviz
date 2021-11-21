{-# LANGUAGE OverloadedStrings #-}

module Configuration.Geometries where

import           Data.Either                    ( partitionEithers )
import qualified Data.List                     as L
import           System.FilePath.Posix          ( (</>) )

import           Codec.Wavefront                ( WavefrontOBJ
                                                , fromFile
                                                )
import           Data.Yaml                      ( (.!=)
                                                , (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                )
import qualified Data.Yaml                     as Y

import           Configuration                  ( loadFolderConfig )
import           Logging                        ( logError )

data OBJGeometryConfig = OBJGeometryConfig
  { geometryName   :: String
  , objData        :: WavefrontOBJ
  , removeCrossbar :: Bool
  }
  deriving (Eq, Show)

data GeometryConfig = GeometryConfig String FilePath Bool
  deriving (Eq, Show)

instance FromJSON GeometryConfig where
  parseJSON (Y.Object v) =
    GeometryConfig
      <$> v
      .:  "name"
      <*> v
      .:  "file"
      <*> v
      .:? "removeCrossbar"
      .!= True
  parseJSON _ = fail "Expected Object for Config value"

newtype GeometryFolderConfig = GeometryFolderConfig
  { geometries :: [GeometryConfig]
  } deriving (Eq, Show)

instance FromJSON GeometryFolderConfig where
  parseJSON (Y.Object v) = GeometryFolderConfig <$> v .: "geometries"
  parseJSON _            = fail "Expected Object for Config value"

loadGeometryFile
  :: FilePath -> GeometryConfig -> IO (Either String OBJGeometryConfig)
loadGeometryFile folderPath (GeometryConfig name objFp rmCross) = do
  fileInput <- fromFile $ folderPath </> objFp
  return $ (\objData -> OBJGeometryConfig name objData rmCross) <$> fileInput

loadGeometryFolder :: FilePath -> IO [Either String OBJGeometryConfig]
loadGeometryFolder folderPath = do
  folderConfig <- loadFolderConfig folderPath
  case folderConfig of
    Left  err -> logError err >> return []
    Right cfg -> mapM (loadGeometryFile folderPath) (geometries cfg)

loadGeometryFolders :: [FilePath] -> IO [OBJGeometryConfig]
loadGeometryFolders folders = do
  (errs, cfgs) <-
    partitionEithers . L.concat <$> mapM loadGeometryFolder folders
  mapM_ logError errs
  return cfgs

