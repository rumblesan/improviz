{-# LANGUAGE OverloadedStrings #-}

module Gfx.Materials
  ( Material(..)
  , MaterialData(..)
  , MaterialLibrary
  , loadMaterial
  , destroyMaterial
  , loadMaterialFile
  , loadMaterialFolder
  , createMaterialLib
  )
where


import qualified Data.Map.Strict               as M
import           Control.Monad                  ( mapM )
import           Data.Maybe                     ( catMaybes )
import           System.FilePath.Posix          ( (</>) )

import           Graphics.Rendering.OpenGL      ( ($=) )
import qualified Graphics.Rendering.OpenGL     as GL

import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                )
import qualified Data.Yaml                     as Y

import           Gfx.LoadShaders

import           Configuration                  ( loadFolderConfig )
import           Configuration.Materials
import           Logging                        ( logError
                                                , logInfo
                                                )

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
  , uniforms :: [(String, GL.UniformLocation)]
  , attributes :: [(String, GL.AttribLocation)]
  } deriving (Show, Eq)

loadMaterial :: MaterialData -> IO Material
loadMaterial md = do
  program <- loadShaders
    [ ShaderInfo GL.VertexShader   (StringSource $ vertexShader md)
    , ShaderInfo GL.FragmentShader (StringSource $ fragmentShader md)
    ]
  GL.currentProgram $= Just program
  uniformInfo <- GL.get $ GL.activeUniforms program
  uniforms <- mapM (\(_, _, uName) -> getUniformLoc program uName) uniformInfo
  attribInfo  <- GL.get $ GL.activeAttribs program
  attributes  <- mapM (\(_, _, aName) -> getAttribLoc program aName) attribInfo
  return $ Material (mdName md) program uniforms attributes

destroyMaterial :: Material -> IO ()
destroyMaterial material = do
  programShaders <- GL.get $ GL.attachedShaders (program material)
  GL.deleteObjectNames programShaders
  GL.deleteObjectName (program material)


getUniformLoc :: GL.Program -> String -> IO (String, GL.UniformLocation)
getUniformLoc p un = do
  ul <- GL.get $ GL.uniformLocation p un
  return (un, ul)

getAttribLoc :: GL.Program -> String -> IO (String, GL.AttribLocation)
getAttribLoc p an = do
  al <- GL.get $ GL.attribLocation p an
  return (an, al)

loadMaterialFile :: FilePath -> IO (Maybe Material)
loadMaterialFile fp = do
  yaml <- Y.decodeFileEither fp
  case yaml of
    Left err -> logError (Y.prettyPrintParseException err) >> return Nothing
    Right matData -> Just <$> loadMaterial matData

loadMaterialFolder :: FilePath -> IO [(String, Material)]
loadMaterialFolder folderPath = do
  folderConfig <- loadFolderConfig folderPath
  materials    <- case folderConfig of
    Left  err -> logError err >> return []
    Right cfg -> mapM ml (materials cfg)
  return $ (\m -> (name m, m)) <$> catMaybes materials
  where ml material = loadMaterialFile (folderPath </> materialFile material)

createMaterialLib :: [FilePath] -> IO MaterialLibrary
createMaterialLib folders = do
  materials <- concat <$> mapM loadMaterialFolder folders
  logInfo $ "Loaded " ++ show (length materials) ++ " material files"
  return $ M.fromList materials
