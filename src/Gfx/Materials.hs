{-# LANGUAGE OverloadedStrings #-}

module Gfx.Materials
  ( Material(..)
  , MaterialData(..)
  , MaterialsConfig(..)
  , MaterialLibrary
  , loadMaterial
  , destroyMaterial
  , loadMaterialString
  , loadMaterialFile
  , loadMaterialFolder
  , createMaterialsConfig
  )
where


import qualified Data.Map.Strict               as M
import           Data.Either                    ( either
                                                , partitionEithers
                                                )
import           Data.ByteString.Lazy.Char8     ( ByteString
                                                , toStrict
                                                )

import           Control.Exception              ( IOException
                                                , try
                                                )
import           Control.Monad                  ( mapM )

import           System.FilePath.Posix          ( (</>) )

import           Graphics.Rendering.OpenGL      ( ($=) )
import qualified Graphics.Rendering.OpenGL     as GL

import qualified Data.Yaml                     as Y
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                )
import Language.Ast                             (Value(..))

import           Gfx.LoadShaders                ( ShaderInfo(..)
                                                , ShaderSource(StringSource)
                                                , loadShaders
                                                )

import           Configuration                  ( loadFolderConfig )
import           Configuration.Materials
import           Logging                        ( logError
                                                , logInfo
                                                )

data MaterialsConfig = MaterialsConfig
  { materialsLibrary :: MaterialLibrary
  , varDefaults :: [(String, Value)]
  }

type MaterialLibrary = M.Map String Material

data MaterialData = MaterialData
  { mdName :: String
  , vertexShader :: String
  , fragmentShader :: String
  }

instance FromJSON MaterialData where
  parseJSON (Y.Object v) =
    MaterialData
      <$> v
      .:  "name"
      <*> v
      .:  "vertexShader"
      <*> v
      .:  "fragmentShader"
  parseJSON _ = fail "Expected Object for Material Data"

data Material = Material
  { name :: String
  , program :: GL.Program
  , uniforms :: [(String, GL.VariableType, GL.UniformLocation)]
  , attributes :: [(String, GL.VariableType, GL.AttribLocation)]
  } deriving (Show, Eq)

loadMaterial :: MaterialData -> IO (Either String Material)
loadMaterial md = do
  result <-
    try $ loadShaders
      [ ShaderInfo GL.VertexShader   (StringSource $ vertexShader md)
      , ShaderInfo GL.FragmentShader (StringSource $ fragmentShader md)
      ] :: IO (Either IOException GL.Program)
  case result of
    Right program -> do
      GL.currentProgram $= Just program
      uniformInfo <- GL.get $ GL.activeUniforms program
      uniforms    <- mapM (getUniformLoc program) uniformInfo
      attribInfo <- GL.get $ GL.activeAttribs program
      attributes <- mapM (getAttribLoc program) attribInfo
      return $ Right $ Material (mdName md) program uniforms attributes
    Left err -> return $ Left (show err)

getUniformLoc :: GL.Program -> (GL.GLint, GL.VariableType, String) -> IO (String, GL.VariableType, GL.UniformLocation)
getUniformLoc p (_, vt, uname) = do
  ul <- GL.get $ GL.uniformLocation p uname
  return (uname, vt, ul)

getAttribLoc :: GL.Program -> (GL.GLint, GL.VariableType, String) -> IO (String, GL.VariableType, GL.AttribLocation)
getAttribLoc p (_, vt, aname) = do
  al <- GL.get $ GL.attribLocation p aname
  return (aname, vt, al)

destroyMaterial :: Material -> IO ()
destroyMaterial material = do
  programShaders <- GL.get $ GL.attachedShaders (program material)
  GL.deleteObjectNames programShaders
  GL.deleteObjectName (program material)

loadMaterialString :: ByteString -> Either String MaterialData
loadMaterialString matStr = either (Left . Y.prettyPrintParseException)
                                   Right
                                   (Y.decodeEither' (toStrict matStr))

loadMaterialFile :: FilePath -> IO (Either String Material)
loadMaterialFile fp = do
  yaml <- Y.decodeFileEither fp
  either (return . Left . Y.prettyPrintParseException) loadMaterial yaml

loadMaterialFolder :: FilePath -> IO ([Either String Material], [(String, Value)])
loadMaterialFolder folderPath = do
  folderConfig <- loadFolderConfig folderPath
  case folderConfig of
    Left err -> logError err >> return ([], [])
    Right cfg -> do
      loadedMaterials <- mapM ml (materialsConfig cfg)
      return (loadedMaterials, folderVarDefaults cfg)
  where ml material = loadMaterialFile (folderPath </> materialFile material)

createMaterialsConfig :: [FilePath] -> IO MaterialsConfig
createMaterialsConfig folders = do
  configs <- mapM loadMaterialFolder folders
  let (errs, materials) = partitionEithers $ concat $ fmap fst configs
  mapM_ logError errs
  let varDefaults = concat $ fmap snd configs
  logInfo $ "Loaded " ++ show (length varDefaults) ++ " material defaults"
  logInfo $ "Loaded " ++ show (length materials) ++ " material files"
  let materialsLib = M.fromList $ (\m -> (name m, m)) <$> materials
  return $ MaterialsConfig materialsLib varDefaults
