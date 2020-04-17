{-# LANGUAGE OverloadedStrings #-}

module Gfx.Materials
  ( Material(..)
  , MaterialData(..)
  , MaterialLibrary
  , loadMaterial
  , destroyMaterial
  , loadMaterialString
  , loadMaterialFile
  , loadMaterialFolder
  , createMaterialLib
  )
where


import           Data.ByteString.Lazy.Char8     ( ByteString
                                                , toStrict
                                                )
import qualified Data.Map.Strict               as M
import           Control.Exception              ( IOException
                                                , try
                                                )
import           Control.Monad                  ( mapM )
import           Data.Either                    ( partitionEithers )
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
      uniforms    <- mapM (\(_, _, uName) -> getUniformLoc program uName)
                          uniformInfo
      attribInfo <- GL.get $ GL.activeAttribs program
      attributes <- mapM (\(_, _, aName) -> getAttribLoc program aName)
                         attribInfo
      return $ Right $ Material (mdName md) program uniforms attributes
    Left err -> return $ Left (show err)

getUniformLoc :: GL.Program -> String -> IO (String, GL.UniformLocation)
getUniformLoc p un = do
  ul <- GL.get $ GL.uniformLocation p un
  return (un, ul)

getAttribLoc :: GL.Program -> String -> IO (String, GL.AttribLocation)
getAttribLoc p an = do
  al <- GL.get $ GL.attribLocation p an
  return (an, al)


destroyMaterial :: Material -> IO ()
destroyMaterial material = do
  programShaders <- GL.get $ GL.attachedShaders (program material)
  GL.deleteObjectNames programShaders
  GL.deleteObjectName (program material)


loadMaterialString :: ByteString -> Either String MaterialData
loadMaterialString matStr = case Y.decodeEither' (toStrict matStr) of
  Left  err     -> Left (Y.prettyPrintParseException err)
  Right matData -> Right matData

loadMaterialFile :: FilePath -> IO (Either String Material)
loadMaterialFile fp = do
  yaml <- Y.decodeFileEither fp
  case yaml of
    Left  err     -> return (Left $ Y.prettyPrintParseException err)
    Right matData -> loadMaterial matData

loadMaterialFolder :: FilePath -> IO [Either String Material]
loadMaterialFolder folderPath = do
  folderConfig <- loadFolderConfig folderPath
  case folderConfig of
    Left  err -> return [Left err]
    Right cfg -> mapM ml (materials cfg)
  where ml material = loadMaterialFile (folderPath </> materialFile material)

createMaterialLib :: [FilePath] -> IO MaterialLibrary
createMaterialLib folders = do
  (errs, materials) <-
    partitionEithers . concat <$> mapM loadMaterialFolder folders
  logInfo $ "Loaded " ++ show (length materials) ++ " material files"
  mapM_ logError errs
  return $ M.fromList $ (\m -> (name m, m)) <$> materials
