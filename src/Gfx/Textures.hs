{-# LANGUAGE OverloadedStrings #-}

module Gfx.Textures where

import qualified Data.Map.Strict           as M

import qualified Data.ByteString           as B
import qualified Data.ByteString.Unsafe    as BSU
import           Data.Maybe                (catMaybes)
import           Data.Yaml                 (FromJSON (..), (.:))
import qualified Data.Yaml                 as Y

import           Codec.BMP
import           Foreign.Ptr
import           Graphics.Rendering.OpenGL as GL

import           System.FilePath.Posix     ((<.>), (</>))

type TextureLibrary = M.Map String TextureObject

data TextureConfig = TextureConfig
  { textureFiles :: [FilePath]
  } deriving (Eq, Show)

instance FromJSON TextureConfig where
  parseJSON (Y.Object v) = TextureConfig <$> v .: "textures"
  parseJSON _            = fail "Expected Object for Config value"

loadTexture :: String -> FilePath -> IO (Maybe (String, TextureObject))
loadTexture name path = do
  img <- readBMP path
  case img of
    Left err -> print ("could not load image: " ++ path) >> return Nothing
    Right image -> do
      text <- genObjectName
      textureBinding Texture2D $= Just text
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      let rgba = unpackBMPToRGBA32 image
          dta = unpackBMPToRGBA32 image
          (width, height) = bmpDimensions image
      bPtr <- BSU.unsafeUseAsCString dta $ \cstr -> return (castPtr cstr)
      let pd = PixelData RGBA UnsignedByte bPtr
      let tSize = TextureSize2D (fromIntegral width) (fromIntegral height)
      texImage2D Texture2D NoProxy 0 RGBA' tSize 0 pd
      textureBinding Texture2D $= Nothing
      return $ Just (name, text)

loadTextureFolder :: FilePath -> IO [(String, TextureObject)]
loadTextureFolder folderPath = do
  let cfgPath = folderPath </> "config.yaml"
  yaml <- B.readFile cfgPath
  case Y.decode yaml of
    Nothing -> print ("Could not read texture config: " ++ cfgPath) >> return []
    Just cfg -> catMaybes <$> mapM tl (textureFiles cfg)
  where
    tl name = loadTexture name (folderPath </> name <.> "bmp")

createTextureLib :: [FilePath] -> IO TextureLibrary
createTextureLib folders =
  M.fromList <$> concat <$> mapM loadTextureFolder folders
