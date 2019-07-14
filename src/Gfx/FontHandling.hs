{-# LANGUAGE TemplateHaskell #-}

module Gfx.FontHandling
  ( loadFont
  , Character(..)
  , Font(..)
  , getCharacter
  )
where

import           Foreign.C.String               ( withCString )
import           Foreign.Marshal.Alloc          ( alloca )
import           Foreign.Ptr                    ( castPtr )
import           Foreign.Storable               ( peek )

import           Control.Monad                  ( unless )
import qualified Data.ByteString               as B
import qualified Data.Char                     as C
import           Data.FileEmbed                 ( embedFile )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( maybe )

import           Graphics.Rendering.FreeType.Internal
                                                ( ft_Done_Face
                                                , ft_Done_FreeType
                                                , ft_Get_Char_Index
                                                , ft_Init_FreeType
                                                , ft_Load_Glyph
                                                , ft_New_Face
                                                , ft_New_Memory_Face
                                                , ft_Render_Glyph
                                                , ft_Set_Pixel_Sizes
                                                )
import qualified Graphics.Rendering.FreeType.Internal.Bitmap
                                               as BM
import           Graphics.Rendering.FreeType.Internal.Face
                                                ( FT_Face )
import qualified Graphics.Rendering.FreeType.Internal.Face
                                               as F
import qualified Graphics.Rendering.FreeType.Internal.GlyphMetrics
                                               as GM
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot
                                               as GS
import           Graphics.Rendering.FreeType.Internal.Library
                                                ( FT_Library )
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
                                                ( FT_Error
                                                , ft_RENDER_MODE_NORMAL
                                                )
import qualified Graphics.Rendering.FreeType.Internal.Size
                                               as FS
import qualified Graphics.Rendering.FreeType.Internal.SizeMetrics
                                               as FSM

import           Graphics.Rendering.OpenGL      ( Clamping(ClampToEdge)
                                                , DataType(UnsignedByte)
                                                , PixelData(PixelData)
                                                , PixelFormat(Red)
                                                , PixelInternalFormat(R8)
                                                , PixelStoreDirection
                                                  ( Pack
                                                  , Unpack
                                                  )
                                                , Proxy(NoProxy)
                                                , Repetition(Repeated)
                                                , TextureCoordName(S, T)
                                                , TextureFilter(Linear')
                                                , TextureObject
                                                , TextureSize2D(TextureSize2D)
                                                , TextureTarget2D(Texture2D)
                                                , ($=)
                                                )
import qualified Graphics.Rendering.OpenGL     as GL

import           Gfx.OpenGL                     ( printErrors )


data Character =
  Character Char -- char
            Int -- width
            Int -- height
            Int -- advance
            Int -- xBearing
            Int -- yBearing
            TextureObject -- texture
  deriving (Eq)

data Font = Font
  { fontCharacters :: M.Map Char Character
  , fontHeight     :: Int
  , fontAscender   :: Int
  , fontAdvance    :: Int
  } deriving (Show)

instance Show Character where
  show (Character c _ _ _ _ _ _) = show c

getCharacter :: Font -> Char -> Maybe Character
getCharacter cm c = M.lookup c (fontCharacters cm)

defaultFont :: B.ByteString
defaultFont = $(embedFile "src/assets/fonts/arial.ttf")

runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
  r <- m
  unless (r == 0) $ fail $ "FreeType Error:" ++ show r

freeType :: IO FT_Library
freeType = alloca $ \p -> do
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
fontFaceFromFile ft fp = withCString fp $ \str -> alloca $ \ptr -> do
  runFreeType $ ft_New_Face ft str 0 ptr
  peek ptr

loadFont :: Maybe FilePath -> Int -> IO Font
loadFont fontPath fontSize =
  let chars = C.chr <$> [0 .. 127]
  in
    do
      ft2  <- freeType
      face <- maybe (fontFaceFromMemory ft2) (fontFaceFromFile ft2) fontPath
      runFreeType $ ft_Set_Pixel_Sizes face (fromIntegral fontSize) 0
      fsize    <- peek $ F.size face
      fmetrics <- peek $ FS.metrics fsize
      GL.rowAlignment Pack $= 1
      c <- sequence $ M.fromList $ fmap (\c -> (c, loadCharacter face c)) chars
      ft_Done_Face face
      ft_Done_FreeType ft2
      return $ Font
        { fontCharacters = c
        , fontHeight     = fromIntegral $ (FSM.height fmetrics `div` 64)
        , fontAscender   = fromIntegral $ (FSM.ascender fmetrics `div` 64)
        , fontAdvance    = fromIntegral $ (FSM.max_advance fmetrics `div` 64)
        }

loadCharacter :: FT_Face -> Char -> IO Character
loadCharacter ff char = do
  chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char
  runFreeType $ ft_Load_Glyph ff chNdx 0
  slot <- peek $ F.glyph ff
  runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
  bmp          <- peek $ GS.bitmap slot
  glyphMetrics <- peek $ GS.metrics slot
  let bmpWidth  = fromIntegral $ BM.width bmp
      bmpHeight = fromIntegral $ BM.rows bmp
      advance   = fromIntegral (GM.horiAdvance glyphMetrics) `div` 64
      xBearing  = fromIntegral $ (GM.horiBearingX glyphMetrics) `div` 64
      yBearing  = fromIntegral $ (GM.horiBearingY glyphMetrics) `div` 64
      gHeight   = fromIntegral $ (GM.height glyphMetrics) `div` 64
      gWidth    = fromIntegral $ (GM.width glyphMetrics) `div` 64
  GL.rowAlignment Unpack $= 1
  text <- GL.genObjectName
  GL.textureBinding Texture2D $= Just text
  printErrors
  let pd    = PixelData Red UnsignedByte (BM.buffer bmp)
  let tSize = TextureSize2D (fromIntegral bmpWidth) (fromIntegral bmpHeight)
  GL.texImage2D Texture2D NoProxy 0 R8 tSize 0 pd
  GL.textureFilter Texture2D $= ((Linear', Nothing), Linear')
  GL.textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  GL.textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  GL.textureBinding Texture2D $= Nothing
  printErrors
  return $ Character char gWidth gHeight advance xBearing yBearing text
