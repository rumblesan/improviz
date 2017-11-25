{-# LANGUAGE TemplateHaskell #-}

module Gfx.FreeType
  ( loadFontCharMap
  , Character(..)
  , CharacterMap(..)
  , getCharacter
  ) where

import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import           Control.Monad
import qualified Data.ByteString                                     as B
import qualified Data.Char                                           as C
import           Data.FileEmbed                                      (embedFile)
import qualified Data.Map.Strict                                     as M
import           Data.Maybe                                          (maybe)

import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.Bitmap         as BM
import           Graphics.Rendering.FreeType.Internal.BitmapGlyph
import           Graphics.Rendering.FreeType.Internal.BitmapSize
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.GlyphMetrics   (horiAdvance,
                                                                      horiBearingX,
                                                                      horiBearingY)
import           Graphics.Rendering.FreeType.Internal.GlyphSlot      as GS
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.FreeType.Internal.Vector

import           Graphics.Rendering.OpenGL                           as GL

import           ErrorHandling                                       (printErrors)

data Character =
  Character Char -- char
            Int -- bpm width
            Int -- bpm height
            Int -- advance
            Int -- xBearing
            Int -- yBearing
            GL.TextureObject -- texture
  deriving (Eq)

data CharacterMap = CharacterMap
  { charMap       :: M.Map Char Character
  , fontHeight    :: Int
  , fontAscender  :: Int
  , fontDescender :: Int
  } deriving (Show)

instance Show Character where
  show (Character c _ _ _ _ _ _) = show c

-- TODO - better error handling
getCharacter :: CharacterMap -> Char -> Character
getCharacter cm c = (charMap cm) M.! c

defaultFont :: B.ByteString
defaultFont = $(embedFile "assets/fonts/arial.ttf")

runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
  r <- m
  unless (r == 0) $ fail $ "FreeType Error:" ++ show r

freeType :: IO FT_Library
freeType =
  alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p

fontFaceFromMemory :: FT_Library -> IO FT_Face
fontFaceFromMemory ft = do
  B.useAsCStringLen defaultFont $ \(cstr, len) -> do
    let custr = castPtr cstr
    alloca $ \ptr -> do
      runFreeType $ ft_New_Memory_Face ft custr (fromIntegral len) 0 ptr
      peek ptr

fontFaceFromFile :: FT_Library -> FilePath -> IO FT_Face
fontFaceFromFile ft fp =
  withCString fp $ \str ->
    alloca $ \ptr -> do
      runFreeType $ ft_New_Face ft str 0 ptr
      peek ptr

setFaceSize :: FT_Face -> Int -> IO ()
setFaceSize ff px = runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0

loadFontCharMap :: Maybe FilePath -> Int -> IO CharacterMap
loadFontCharMap fontPath fontSize =
  let chars = C.chr <$> [0 .. 127]
  in do ft2 <- freeType
        face <- maybe (fontFaceFromMemory ft2) (fontFaceFromFile ft2) fontPath
        setFaceSize face fontSize
        GL.rowAlignment GL.Pack $= 1
        c <-
          sequence $ M.fromList $ fmap (\c -> (c, loadCharacter face c)) chars
        fdsc <- peek $ descender face
        fasc <- peek $ ascender face
        ft_Done_Face face
        ft_Done_FreeType ft2
        return $
          CharacterMap
          { charMap = c
          , fontHeight = fontSize
          , fontAscender = fromIntegral fasc
          , fontDescender = fromIntegral fdsc
          }

loadCharacter :: FT_Face -> Char -> IO Character
loadCharacter ff char = do
  chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char
  runFreeType $ ft_Load_Glyph ff chNdx 0
  slot <- peek $ glyph ff
  runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
  bmp <- peek $ GS.bitmap slot
  glyphMetrics <- peek $ GS.metrics slot
  let bmpWidth = fromIntegral $ BM.width bmp
      bmpHeight = fromIntegral $ BM.rows bmp
      advance = fromIntegral (horiAdvance glyphMetrics) `div` 64
      xBearing = fromIntegral $ (horiBearingX glyphMetrics) `div` 64
      yBearing = fromIntegral $ (horiBearingY glyphMetrics) `div` 64
  rowAlignment Unpack $= 1
  text <- genObjectName
  textureBinding Texture2D $= Just text
  printErrors
  let pd = PixelData Red UnsignedByte (BM.buffer bmp)
  let tSize = TextureSize2D (fromIntegral bmpWidth) (fromIntegral bmpHeight)
  texImage2D Texture2D NoProxy 0 R8 tSize 0 pd
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  textureBinding Texture2D $= Nothing
  printErrors
  return $ Character char bmpWidth bmpHeight advance xBearing yBearing text
