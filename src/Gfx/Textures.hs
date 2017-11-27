{-# LANGUAGE OverloadedStrings #-}

module Gfx.Textures where

import qualified Data.ByteString           as B
import qualified Data.Map.Strict           as M
import           Data.Maybe                (catMaybes)
import           Data.Vector.Storable      (unsafeWith)

import           Foreign.Ptr               (castPtr)

import           Data.Yaml                 (FromJSON (..), (.:))
import qualified Data.Yaml                 as Y

import           Codec.Picture             (readImage)
import           Codec.Picture.Bitmap      (decodeBitmap)
import           Codec.Picture.Gif         (decodeGif)
import           Codec.Picture.Types       (DynamicImage (ImageRGB8, ImageRGBA8),
                                            Image (..))
import           Graphics.Rendering.OpenGL (DataType (UnsignedByte),
                                            PixelData (..),
                                            PixelFormat (RGB, RGBA),
                                            PixelInternalFormat (RGB', RGBA'),
                                            Proxy (NoProxy), TextureObject,
                                            TextureSize2D (..),
                                            TextureTarget2D (Texture2D), ($=))
import qualified Graphics.Rendering.OpenGL as GL
import           System.FilePath.Posix     (takeExtension, (<.>), (</>))

type TextureLibrary = M.Map String TextureObject

data TextureConfig = TextureConfig
  { textureName :: String
  , textureFile :: FilePath
  } deriving (Eq, Show)

instance FromJSON TextureConfig where
  parseJSON (Y.Object v) = TextureConfig <$> v .: "name" <*> v .: "file"
  parseJSON _            = fail "Expected Object for Config value"

data TextureFolderConfig = TextureFolderConfig
  { textures :: [TextureConfig]
  } deriving (Eq, Show)

instance FromJSON TextureFolderConfig where
  parseJSON (Y.Object v) = TextureFolderConfig <$> v .: "textures"
  parseJSON _            = fail "Expected Object for Config value"

loadTexture :: String -> FilePath -> IO (Maybe (String, TextureObject))
loadTexture name path = do
  imgData <- B.readFile path
  case takeExtension path of
    ".bmp" -> do
      loaded <- either (return . Left) handleImage (decodeBitmap imgData)
      either
        (\err ->
           print ("could not load image " ++ path ++ ": " ++ err) >>
           return Nothing)
        (\i -> return $ Just (name, i))
        loaded
    ext -> print ("Unknown extension: " ++ ext) >> return Nothing

handleImage :: DynamicImage -> IO (Either String TextureObject)
handleImage (ImageRGBA8 (Image width height dat)) = do
  text <- GL.genObjectName
  GL.textureBinding Texture2D $= Just text
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  unsafeWith dat $ \ptr -> do
    GL.texImage2D
      Texture2D
      NoProxy
      0
      RGBA'
      (TextureSize2D (fromIntegral width) (fromIntegral height))
      0
      (PixelData RGBA UnsignedByte ptr)
    GL.textureBinding Texture2D $= Nothing
    return $ Right text
handleImage (ImageRGB8 (Image width height dat)) = do
  text <- GL.genObjectName
  GL.textureBinding Texture2D $= Just text
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  unsafeWith dat $ \ptr -> do
    GL.texImage2D
      Texture2D
      NoProxy
      0
      RGB'
      (TextureSize2D (fromIntegral width) (fromIntegral height))
      0
      (PixelData RGB UnsignedByte ptr)
    GL.textureBinding Texture2D $= Nothing
    return $ Right text
handleImage _ = return $ Left "Unsupported Format"

loadTextureFolder :: FilePath -> IO [(String, TextureObject)]
loadTextureFolder folderPath = do
  yaml <- Y.decodeFileEither $ folderPath </> "config.yaml"
  case yaml of
    Left err  -> print err >> return []
    Right cfg -> catMaybes <$> mapM tl (textures cfg)
  where
    tl texture =
      loadTexture (textureName texture) (folderPath </> (textureFile texture))

createTextureLib :: [FilePath] -> IO TextureLibrary
createTextureLib folders =
  M.fromList <$> concat <$> mapM loadTextureFolder folders
