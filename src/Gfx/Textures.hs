{-# LANGUAGE OverloadedStrings #-}

module Gfx.Textures where

import           Data.ByteString           (useAsCString)
import qualified Data.Map.Strict           as M
import           Data.Maybe                (catMaybes)

import           Foreign.Ptr               (castPtr)

import           Data.Yaml                 (FromJSON (..), (.:))
import qualified Data.Yaml                 as Y

import qualified Codec.BMP                 as BMP
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
  img <- BMP.readBMP path
  case img of
    Left err -> print ("could not load image: " ++ path) >> return Nothing
    Right image -> do
      text <- genObjectName
      textureBinding Texture2D $= Just text
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      let dta = BMP.unpackBMPToRGBA32 image
          (width, height) = BMP.bmpDimensions image
      useAsCString dta $ \cstr -> do
        let bPtr = castPtr cstr
        let pd = PixelData RGBA UnsignedByte bPtr
        let tSize = TextureSize2D (fromIntegral width) (fromIntegral height)
        texImage2D Texture2D NoProxy 0 RGBA' tSize 0 pd
        textureBinding Texture2D $= Nothing
        return $ Just (name, text)

loadTextureFolder :: FilePath -> IO [(String, TextureObject)]
loadTextureFolder folderPath = do
  yaml <- Y.decodeFileEither $ folderPath </> "config.yaml"
  case yaml of
    Left err  -> print err >> return []
    Right cfg -> catMaybes <$> mapM tl (textureFiles cfg)
  where
    tl name = loadTexture name (folderPath </> name <.> "bmp")

createTextureLib :: [FilePath] -> IO TextureLibrary
createTextureLib folders =
  M.fromList <$> concat <$> mapM loadTextureFolder folders
