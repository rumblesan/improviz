{-# LANGUAGE OverloadedStrings #-}

module Gfx.Materials
  ( Material(..)
  , MaterialData(..)
  , MaterialsConfig(..)
  , MaterialLibrary
  , loadMaterial
  , destroyMaterial
  , createMaterialsConfig
  ) where


import           Data.Either                    ( partitionEithers )
import qualified Data.Map.Strict               as M

import           Control.Exception              ( IOException
                                                , try
                                                )
import           Control.Monad                  ( mapM )

import           Graphics.Rendering.OpenGL      ( ($=) )
import qualified Graphics.Rendering.OpenGL     as GL

import qualified Data.Yaml                     as Y
import           Data.Yaml                      ( (.:)
                                                , FromJSON(..)
                                                )
import           Language.Ast                   ( Value(..) )

import           Gfx.LoadShaders                ( ShaderInfo(..)
                                                , ShaderSource(StringSource)
                                                , loadShaders
                                                )
import           Gfx.Shaders                    ( getAttribLoc
                                                , getUniformLoc
                                                )

import           Configuration.Shaders
import           Logging                        ( logError
                                                , logInfo
                                                )

data MaterialsConfig = MaterialsConfig
  { materialsLibrary :: MaterialLibrary
  , varDefaults      :: [(String, Value)]
  }

type MaterialLibrary = M.Map String Material

data MaterialData = MaterialData
  { mdName         :: String
  , vertexShader   :: String
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
  { name       :: String
  , program    :: GL.Program
  , uniforms   :: [(String, GL.VariableType, GL.UniformLocation)]
  , attributes :: [(String, GL.VariableType, GL.AttribLocation)]
  }
  deriving (Show, Eq)

loadMaterial :: ShaderData -> IO (Either String Material)
loadMaterial (ShaderData matName vertShader fragShader) = do
  result <-
    try $ loadShaders
      [ ShaderInfo GL.VertexShader   (StringSource vertShader)
      , ShaderInfo GL.FragmentShader (StringSource fragShader)
      ] :: IO (Either IOException GL.Program)
  case result of
    Right program -> do
      GL.currentProgram $= Just program
      uniformInfo <- GL.get $ GL.activeUniforms program
      uniforms    <- mapM (getUniformLoc program) uniformInfo
      attribInfo  <- GL.get $ GL.activeAttribs program
      attributes  <- mapM (getAttribLoc program) attribInfo
      logInfo $ "Loading " ++ matName ++ " material"
      return $ Right $ Material matName program uniforms attributes
    Left err -> return $ Left (show err)

destroyMaterial :: Material -> IO ()
destroyMaterial material = do
  programShaders <- GL.get $ GL.attachedShaders (program material)
  GL.deleteObjectNames programShaders
  GL.deleteObjectName (program material)

createMaterialsConfig :: [FilePath] -> IO MaterialsConfig
createMaterialsConfig folders = do
  loadedFolders <- mapM loadShaderFolder folders
  let (folderLoadingErrs, parsedShaders) =
        partitionEithers $ concat $ fmap fst loadedFolders
  mapM_ logError folderLoadingErrs
  loadedMaterials <- mapM loadMaterial parsedShaders
  let (materialLoadingErrs, materials) = partitionEithers loadedMaterials
  mapM_ logError materialLoadingErrs
  let varDefaults = concat $ fmap snd loadedFolders
  logInfo $ "Loaded " ++ show (length varDefaults) ++ " material defaults"
  logInfo $ "Loaded " ++ show (length materials) ++ " material files"
  let materialsLib = M.fromList $ (\m -> (name m, m)) <$> materials
  return $ MaterialsConfig materialsLib varDefaults


