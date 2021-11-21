{-# LANGUAGE OverloadedStrings #-}

module Configuration.Shaders where

import           Data.ByteString.Lazy.Char8     ( ByteString
                                                , toStrict
                                                )
import qualified Data.Yaml                     as Y
import           System.FilePath.Posix          ( (</>) )

import           Configuration                  ( loadFolderConfig )
import           Configuration.Ast              ( )
import           Data.Maybe                     ( fromMaybe )
import           Data.Yaml                      ( (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                )
import           Language.Ast                   ( Value(..) )

data ShaderConfig = ShaderConfig
  { shaderName :: String
  , shaderFile :: FilePath
  }
  deriving (Eq, Show)

instance FromJSON ShaderConfig where
  parseJSON (Y.Object v) = ShaderConfig <$> v .: "name" <*> v .: "file"
  parseJSON _            = fail "Expected Object for Config value"

data ShaderFolderConfig = ShaderFolderConfig
  { shadersConfig     :: [ShaderConfig]
  , folderVarDefaults :: [(String, Value)]
  }
  deriving (Eq, Show)

instance FromJSON ShaderFolderConfig where
  parseJSON (Y.Object v) = do
    shaders       <- v .: "shaders"
    defaultsArray <- v .:? "defaults"
    varDefaults   <- mapM (\o -> (,) <$> o .: "name" <*> o .: "value")
                          (fromMaybe [] defaultsArray)
    return $ ShaderFolderConfig shaders varDefaults
  parseJSON _ = fail "Expected Object for Config value"

data ShaderData = ShaderData
  { sdName           :: String
  , sdvertexShader   :: String
  , sdfragmentShader :: String
  }

instance FromJSON ShaderData where
  parseJSON (Y.Object v) =
    ShaderData <$> v .: "name" <*> v .: "vertexShader" <*> v .: "fragmentShader"
  parseJSON _ = fail "Expected Object for Shader Data"

loadShaderString :: ByteString -> Either String ShaderData
loadShaderString matStr = either (Left . Y.prettyPrintParseException)
                                 Right
                                 (Y.decodeEither' (toStrict matStr))

loadShaderFile :: FilePath -> IO (Either String ShaderData)
loadShaderFile fp = do
  yaml <- Y.decodeFileEither fp
  return $ either (Left . Y.prettyPrintParseException) Right yaml

loadShaderFolder
  :: FilePath -> IO ([Either String ShaderData], [(String, Value)])
loadShaderFolder folderPath = do
  folderConfig <- loadFolderConfig folderPath
  case folderConfig of
    Left  err -> return ([Left err], [])
    Right (ShaderFolderConfig shadersCfg varDefaults) -> do
      loadedShaders <- mapM load shadersCfg
      return (loadedShaders, varDefaults)
 where
  load (ShaderConfig _ filePath) = loadShaderFile (folderPath </> filePath)

