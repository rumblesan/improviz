{-# LANGUAGE OverloadedStrings #-}

module Gfx.Textures where

import qualified Data.Map.Strict           as M

import qualified Data.ByteString           as B
import qualified Data.ByteString.Unsafe    as BSU
import           Data.Yaml                 (FromJSON (..), (.:))
import qualified Data.Yaml                 as Y

import           Codec.BMP
import           Foreign.Ptr
import           Graphics.Rendering.OpenGL as GL

type TextureLibrary = M.Map String TextureObject

data TextureConfig = TextureConfig
  { textureFiles :: [FilePath]
  } deriving (Eq, Show)

instance FromJSON TextureConfig where
  parseJSON (Y.Object v) = TextureConfig <$> v .: "textures"
  parseJSON _            = fail "Expected Object for Config value"

loadTexture :: String -> FilePath -> IO (String, TextureObject)
loadTexture name path = do
  text <- genObjectName
  textureBinding Texture2D $= Just text
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  Right image <- readBMP path
  let rgba = unpackBMPToRGBA32 image
      dta = unpackBMPToRGBA32 image
      (width, height) = bmpDimensions image
  bPtr <- BSU.unsafeUseAsCString dta $ \cstr -> return (castPtr cstr)
  let pd = PixelData RGBA UnsignedByte bPtr
  let tSize = TextureSize2D (fromIntegral width) (fromIntegral height)
  texImage2D Texture2D NoProxy 0 RGBA' tSize 0 pd
  textureBinding Texture2D $= Nothing
  return (name, text)

createTextureLib :: IO TextureLibrary
createTextureLib = do
  yaml <- B.readFile "./textures/config.yaml"
  let config = Y.decode yaml :: Maybe TextureConfig
  case config of
    Nothing -> error "Could not read texture config"
    Just cfg -> do
      textures <- mapM tl (textureFiles cfg)
      return $ M.fromList textures
  where
    tl name = loadTexture name ("textures/" ++ name ++ ".bmp")
