module Gfx.FreeType where

import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import           Control.Monad
import qualified Data.Char                                           as C
import qualified Data.Map.Strict                                     as M

import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.Bitmap         as BM
import           Graphics.Rendering.FreeType.Internal.BitmapGlyph
import           Graphics.Rendering.FreeType.Internal.BitmapSize
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.GlyphSlot      as GS
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.FreeType.Internal.Vector

import           Graphics.Rendering.OpenGL                           as GL

import           ErrorHandling                                       (printErrors)

data Character =
  Character Char
            Int
            Int
            Int
            GL.TextureObject
  deriving (Eq)

type CharacterMap = M.Map Char Character

instance Show Character where
  show (Character c _ _ _ _) = show c

runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
  r <- m
  unless (r == 0) $ fail $ "FreeType Error:" ++ show r

freeType :: IO FT_Library
freeType =
  alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p

fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp =
  withCString fp $ \str ->
    alloca $ \ptr -> do
      runFreeType $ ft_New_Face ft str 0 ptr
      peek ptr

setFaceSize :: FT_Face -> Int -> IO ()
setFaceSize ff px = runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0

loadFontCharMap :: String -> Int -> IO CharacterMap
loadFontCharMap fontPath fontSize =
  let chars = C.chr <$> [0 .. 127]
  in do ft2 <- freeType
        face <- fontFace ft2 fontPath
        setFaceSize face fontSize
        GL.rowAlignment GL.Pack $= 1
        c <-
          sequence $ M.fromList $ fmap (\c -> (c, loadCharacter face c)) chars
        ft_Done_Face face
        ft_Done_FreeType ft2
        return c

loadCharacter :: FT_Face -> Char -> IO Character
loadCharacter ff char = do
  chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char
  runFreeType $ ft_Load_Glyph ff chNdx 0
  slot <- peek $ glyph ff
  runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
  bmp <- peek $ GS.bitmap slot
  (FT_Vector advx advy) <- peek $ GS.advance slot
  let bmpWidth = fromIntegral $ BM.width bmp
      bmpHeight = fromIntegral $ BM.rows bmp
      advance = fromIntegral advx `div` 64
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
  return $ Character char bmpWidth bmpHeight advance text
