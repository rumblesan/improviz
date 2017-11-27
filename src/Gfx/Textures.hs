{-# LANGUAGE OverloadedStrings #-}

module Gfx.Textures where

import qualified Data.Map.Strict           as M
import           Data.Maybe                (catMaybes)
import           Data.Vector.Storable      (unsafeWith)

import           Foreign.Ptr               (castPtr)

import           Data.Yaml                 (FromJSON (..), (.:))
import qualified Data.Yaml                 as Y

import           Codec.Picture             (readImage)
import           Codec.Picture.Types       (DynamicImage (..), Image (..))
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
  img <- readImage path
  case img of
    Left err    -> print ("could not load image: " ++ path) >> return Nothing
    Right image -> handleImage name image

handleImage :: String -> DynamicImage -> IO (Maybe (String, TextureObject))
handleImage name (ImageRGBA8 (Image width height dat)) = do
  text <- genObjectName
  textureBinding Texture2D $= Just text
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  unsafeWith dat $ \ptr -> do
    texImage2D
      Texture2D
      NoProxy
      0
      RGBA'
      (TextureSize2D (fromIntegral width) (fromIntegral height))
      0
      (PixelData RGBA UnsignedByte ptr)
    textureBinding Texture2D $= Nothing
    return $ Just (name, text)
handleImage name _ = print ("Could not load image: " ++ name) >> return Nothing

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
