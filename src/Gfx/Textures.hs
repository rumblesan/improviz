{-# LANGUAGE OverloadedStrings #-}

module Gfx.Textures
  ( TextureConfig(..)
  , TextureFolderConfig(..)
  , TextureLibrary
  , createTextureLib
  , addTexture
  ) where

import qualified Data.ByteString           as B
import           Data.Either               (rights)
import           Data.List                 (concat)
import qualified Data.Map.Strict           as M
import           Data.Maybe                (catMaybes)
import           Data.Vector.Storable      (unsafeWith)

import           Foreign.Ptr               (castPtr)

import           Data.Yaml                 (FromJSON (..), (.:))
import qualified Data.Yaml                 as Y

import           Codec.Picture             (Pixel, decodeImage)
import           Codec.Picture.Gif         (decodeGifImages)
import           Codec.Picture.Types       (DynamicImage (..), Image (..))
import           Graphics.Rendering.OpenGL (DataType (UnsignedByte),
                                            PixelData (..), PixelFormat (..),
                                            PixelInternalFormat (..),
                                            Proxy (NoProxy), TextureObject,
                                            TextureSize2D (..),
                                            TextureTarget2D (Texture2D), ($=))
import qualified Graphics.Rendering.OpenGL as GL
import           System.FilePath.Posix     (takeExtension, (<.>), (</>))

import           Logging                   (logError, logInfo)

type TextureLibrary = M.Map String (M.Map Int TextureObject)

data TextureConfig = TextureConfig
  { textureName :: String
  , textureFile :: FilePath
  } deriving (Eq, Show)

instance FromJSON TextureConfig where
  parseJSON (Y.Object v) = TextureConfig <$> v .: "name" <*> v .: "file"
  parseJSON _            = fail "Expected Object for Config value"

newtype TextureFolderConfig = TextureFolderConfig
  { textures :: [TextureConfig]
  } deriving (Eq, Show)

instance FromJSON TextureFolderConfig where
  parseJSON (Y.Object v) = TextureFolderConfig <$> v .: "textures"
  parseJSON _            = fail "Expected Object for Config value"

loadTexture :: String -> FilePath -> IO (String, M.Map Int TextureObject)
loadTexture name path = do
  imgData <- B.readFile path
  case takeExtension path of
    ".gif" ->
      case decodeGifImages imgData of
        Left err ->
          logError ("could not load image " ++ path ++ ": " ++ err) >>
          return (name, M.empty)
        Right images -> do
          i <- rights <$> mapM handleImage images
          return (name, M.fromList $ zipWith (\idx t -> (idx, t)) [0 ..] i)
    ext -> do
      loaded <- either (return . Left) handleImage (decodeImage imgData)
      either
        (\err ->
           logError ("could not load image " ++ path ++ ": " ++ err) >>
           return (name, M.empty))
        (\i -> return (name, M.singleton 0 i))
        loaded

handleImage :: DynamicImage -> IO (Either String TextureObject)
handleImage img =
  case img of
    ImageY8 _       -> return $ Left "ImageY8: Unsupported Format"
    ImageY16 _      -> return $ Left "ImageY16: Unsupported Format"
    ImageYF _       -> return $ Left "ImageYF: Unsupported Format"
    ImageYA8 _      -> return $ Left "ImageYA8: Unsupported Format"
    ImageYA16 _     -> return $ Left "ImageYA16: Unsupported Format"
    ImageRGB8 img   -> imgToTexture img RGB RGB'
    ImageRGB16 img  -> imgToTexture img RGB RGB'
    ImageRGBF img   -> imgToTexture img RGB RGB32F
    ImageRGBA8 img  -> imgToTexture img RGBA RGBA'
    ImageRGBA16 img -> imgToTexture img RGBA RGBA'
    ImageYCbCr8 img -> imgToTexture img YCBCR422 RGB'
    ImageCMYK8 _    -> return $ Left "ImageCMYK8: Unsupported Format"
    ImageCMYK16 _   -> return $ Left "ImageCMYK16: Unsupported Format"
  where
    imgToTexture ::
         (Pixel a)
      => Image a
      -> PixelFormat
      -> PixelInternalFormat
      -> IO (Either String TextureObject)
    imgToTexture image format internalFormat = do
      text <- GL.genObjectName
      GL.textureBinding Texture2D $= Just text
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      unsafeWith (imageData image) $ \ptr ->
        GL.texImage2D
          Texture2D
          NoProxy
          0
          internalFormat
          (TextureSize2D
             (fromIntegral $ imageWidth image)
             (fromIntegral $ imageHeight image))
          0
          (PixelData format UnsignedByte ptr)
      GL.textureBinding Texture2D $= Nothing
      return $ Right text

loadTextureFolder :: FilePath -> IO [(String, M.Map Int TextureObject)]
loadTextureFolder folderPath = do
  yaml <- Y.decodeFileEither $ folderPath </> "config.yaml"
  case yaml of
    Left err  -> logError (show err) >> return []
    Right cfg -> mapM tl (textures cfg)
  where
    tl texture =
      loadTexture (textureName texture) (folderPath </> textureFile texture)

createTextureLib :: [FilePath] -> IO TextureLibrary
createTextureLib folders = do
  textures <- concat <$> mapM loadTextureFolder folders
  logInfo $ "Loaded " ++ show (length textures) ++ " texture files"
  return $ M.fromList textures

addTexture :: TextureLibrary -> String -> TextureObject -> TextureLibrary
addTexture lib name tobj = M.insert name (M.singleton 0 tobj) lib
