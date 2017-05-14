module Gfx.TextRendering (
    createTextRenderer
  , renderText
  , renderTextbuffer
  , renderCharacter
  , resizeTextRendererScreen
  , changeTextColour
  , textCoordMatrix
  , TextRenderer
) where


import Control.Monad

import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr

import qualified Data.Map.Strict as M

import Graphics.Rendering.OpenGL as GL
import Gfx.GeometryBuffers (bufferOffset)
import Gfx.LoadShaders
import Gfx.FreeType (CharacterMap, Character(..), loadFontCharMap)
import qualified Graphics.GL as GLRaw

import Gfx.PostProcessing (Savebuffer(..), createSavebuffer, drawQuadVBO)

import Data.Vec (Mat44, multmm)
import Gfx.Matrices

import ErrorHandling (printErrors)


data CharQuad = CharQuad VertexArrayObject BufferObject ArrayIndex NumArrayIndices deriving (Show, Eq)

data TextRenderer = TextRenderer {
    characterMap :: CharacterMap
  , charSize :: Int
  , pMatrix :: Mat44 GLfloat
  , program :: Program
  , characterQuad :: CharQuad
  , textColour :: Color4 GLfloat
  , textBGcolour :: Color4 GLfloat
  , outbuffer :: Savebuffer
} deriving Show

textCoordMatrix :: Floating f => f -> f -> f -> f -> f -> f -> Mat44 f
textCoordMatrix left right top bottom near far =
  let
    o = orthographicMat left right top bottom near far
    t = translateMat (-1) 1 0
  in
    multmm t o

-- TODO - better error handling
getCharacter :: TextRenderer -> Char -> Character
getCharacter tr c = characterMap tr M.! c

charQuad :: IO CharQuad
charQuad = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  let
    vertexSize = sizeOf (0 :: GLfloat)
    firstPosIndex = 0
    firstTexIndex = 2 * vertexSize
    vPosition = AttribLocation 0
    vTexCoord = AttribLocation 1
    numVertices = 6 * 4
    size = fromIntegral (numVertices * vertexSize)
    stride = fromIntegral (4 * vertexSize)
  bufferData ArrayBuffer $= (size, nullPtr, DynamicDraw)
  vertexAttribPointer vPosition $= (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstPosIndex))
  vertexAttribPointer vTexCoord $= (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstTexIndex))
  vertexAttribArray vPosition $= Enabled
  vertexAttribArray vTexCoord $= Enabled
  bindVertexArrayObject $= Nothing
  bindBuffer ArrayBuffer $= Nothing

  return $ CharQuad vao arrayBuffer firstPosIndex 6

drawCharQuad :: CharQuad -> IO ()
drawCharQuad (CharQuad arrayObject _ firstIndex numTriangles) = do
  bindVertexArrayObject $= Just arrayObject
  drawArrays Triangles firstIndex numTriangles

createTextRenderer :: Float -> Float -> Int -> Int -> String -> Int -> Color4 GLfloat -> Color4 GLfloat -> IO TextRenderer
createTextRenderer front back width height fontPath charSize textColour bgColour = do
  cq <- charQuad
  program <- loadShaders [
    ShaderInfo VertexShader (FileSource "shaders/textrenderer.vert"),
    ShaderInfo FragmentShader (FileSource "shaders/textrenderer.frag")]
  characters <- loadFontCharMap fontPath charSize
  let projectionMatrix = textCoordMatrix 0 (fromIntegral width) 0 (fromIntegral height) front back
  buffer <- createSavebuffer (fromIntegral width) (fromIntegral height)
  return $ TextRenderer characters charSize projectionMatrix program cq textColour bgColour buffer

resizeTextRendererScreen :: Mat44 GLfloat -> TextRenderer -> TextRenderer
resizeTextRendererScreen orthoMatrix trender =
  trender {
    pMatrix = orthoMatrix
  }

changeTextColour :: Color4 GLfloat -> TextRenderer -> TextRenderer
changeTextColour newColour trender =
  trender {
    textColour = newColour
  }

renderText :: Int -> Int -> TextRenderer -> String -> IO ()
renderText xpos ypos renderer strings = do
  let (Savebuffer fbo _ _ _ _) = outbuffer renderer
  bindFramebuffer Framebuffer $= fbo
  blend $= Enabled
  blendEquationSeparate $= (FuncAdd, FuncAdd)
  blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
  depthFunc $= Nothing
  clearColor $= Color4 0.0 1.0 0.0 0.0
  clear [ ColorBuffer ]
  currentProgram $= Just (program renderer)
  foldM_ (\(xp, yp) c ->
    case c of
      '\n' -> return (xpos, yp + charSize renderer)
      _ ->
        do
          let char@(Character _ _ _ adv _) = getCharacter renderer c
          renderCharacter renderer char xp yp
          return (xp + adv, yp)
    ) (xpos, ypos) strings
  printErrors

renderTextbuffer :: TextRenderer -> IO ()
renderTextbuffer renderer = do
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  let (Savebuffer _ text _ program quadVBO) = outbuffer renderer
  currentProgram $= Just program
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just text
  drawQuadVBO quadVBO

renderCharacter :: TextRenderer -> Character -> Int -> Int -> IO ()
renderCharacter renderer (Character c width height adv text) x y =
  let
    xPos = fromIntegral x
    yPos = fromIntegral y
    lbottom = yPos + fromIntegral (charSize renderer) :: GLfloat
    ltop = lbottom - fromIntegral height :: GLfloat
    w = fromIntegral width
    h = fromIntegral height
    top = yPos + fromIntegral (charSize renderer) - fromIntegral height
    charVerts = [
      xPos, ltop,         0.0, 0.0,
      xPos, lbottom,      0.0, 1.0,
      xPos + w, ltop,     1.0, 0.0,

      xPos, lbottom,      0.0, 1.0,
      xPos + w, lbottom,  1.0, 1.0,
      xPos + w, ltop,     1.0, 0.0] :: [GLfloat]
    vertSize = sizeOf (head charVerts)
    numVerts = length charVerts
    size = fromIntegral (numVerts * vertSize)
    (CharQuad arrayObject arrayBuffer firstIndex numTriangles) = characterQuad renderer
  in
    do
      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just text

      bindVertexArrayObject $= Just arrayObject
      bindBuffer ArrayBuffer $= Just arrayBuffer

      textColourU <- GL.get $ uniformLocation (program renderer) "textColor"
      uniform textColourU $= textColour renderer

      (UniformLocation projU) <- GL.get $ uniformLocation (program renderer) "projection"
      with (pMatrix renderer)
        $ GLRaw.glUniformMatrix4fv projU 1 (fromBool True)
        . castPtr
      withArray charVerts $ \ptr ->
        bufferSubData ArrayBuffer WriteToBuffer 0 size ptr
      drawArrays Triangles firstIndex numTriangles
      printErrors

