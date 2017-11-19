module Gfx.Textures where

import qualified Data.Map.Strict           as M

import qualified Data.ByteString           as B
import qualified Data.ByteString.Unsafe    as BSU

import           Codec.BMP
import           Foreign.Ptr
import           Graphics.Rendering.OpenGL as GL

type TextureLibrary = M.Map String TextureObject

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
  texture <- loadTexture "crystal" "textures/crystal.bmp"
  return $ M.fromList [texture]
