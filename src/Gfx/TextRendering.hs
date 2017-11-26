module Gfx.TextRendering
  ( createTextRenderer
  , renderText
  , renderTextbuffer
  , renderCharacter
  , resizeTextRendererScreen
  , changeTextColour
  , textCoordMatrix
  , TextRenderer
  ) where

import           Control.Monad             (foldM_)

import           Foreign.Marshal.Array     (withArray)
import           Foreign.Marshal.Utils     (fromBool, with)
import           Foreign.Ptr               (castPtr, nullPtr)
import           Foreign.Storable          (peek, sizeOf)

import qualified Data.Map.Strict           as M

import           Gfx.FontHandling          (Character (..), Font (..),
                                            getCharacter, loadFontCharMap)
import           Gfx.GeometryBuffers       (bufferOffset)
import           Gfx.LoadShaders           (ShaderInfo (..), ShaderSource (..),
                                            loadShaders)
import qualified Graphics.GL               as GLRaw
import           Graphics.Rendering.OpenGL (ArrayIndex, AttribLocation (..),
                                            BlendEquation (FuncAdd),
                                            BlendingFactor (One, OneMinusSrcAlpha, SrcAlpha, Zero),
                                            BufferObject,
                                            BufferTarget (ArrayBuffer),
                                            BufferUsage (DynamicDraw),
                                            Capability (Enabled),
                                            ClearBuffer (ColorBuffer),
                                            Color4 (..), Color4,
                                            DataType (Float),
                                            FramebufferTarget (Framebuffer),
                                            GLfloat, IntegerHandling (ToFloat),
                                            NumArrayIndices,
                                            PrimitiveMode (Triangles), Program,
                                            ShaderType (FragmentShader, VertexShader),
                                            TextureTarget2D (Texture2D),
                                            TextureUnit (..),
                                            TransferDirection (WriteToBuffer),
                                            UniformLocation (..),
                                            VertexArrayDescriptor (..),
                                            VertexArrayObject, ($=))
import qualified Graphics.Rendering.OpenGL as GL

import           Gfx.PostProcessing        (Savebuffer (..), createSavebuffer,
                                            deleteSavebuffer, drawQuadVBO)

import           Data.Vec                  (Mat44, multmm)
import           Gfx.Matrices              (orthographicMat, translateMat)

import           ErrorHandling             (printErrors)

data CharQuad =
  CharQuad VertexArrayObject
           BufferObject
           ArrayIndex
           NumArrayIndices
  deriving (Show, Eq)

data TextRenderer = TextRenderer
  { textFont        :: Font
  , charSize        :: Int
  , pMatrix         :: Mat44 GLfloat
  , textprogram     :: Program
  , bgprogram       :: Program
  , characterQuad   :: CharQuad
  , characterBGQuad :: CharQuad
  , textColour      :: Color4 GLfloat
  , textBGColour    :: Color4 GLfloat
  , outbuffer       :: Savebuffer
  } deriving (Show)

textCoordMatrix :: Floating f => f -> f -> f -> f -> f -> f -> Mat44 f
textCoordMatrix left right top bottom near far =
  let o = orthographicMat left right top bottom near far
      t = translateMat (-1) 1 0
  in multmm t o

charQuad :: IO CharQuad
charQuad = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  arrayBuffer <- GL.genObjectName
  GL.bindBuffer ArrayBuffer $= Just arrayBuffer
  let vertexSize = sizeOf (0 :: GLfloat)
      firstPosIndex = 0
      firstTexIndex = 2 * vertexSize
      vPosition = AttribLocation 0
      vTexCoord = AttribLocation 1
      numVertices = 6 * 4
      size = fromIntegral (numVertices * vertexSize)
      stride = fromIntegral (4 * vertexSize)
  GL.bufferData ArrayBuffer $= (size, nullPtr, DynamicDraw)
  GL.vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstPosIndex))
  GL.vertexAttribPointer vTexCoord $=
    (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstTexIndex))
  GL.vertexAttribArray vPosition $= Enabled
  GL.vertexAttribArray vTexCoord $= Enabled
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer ArrayBuffer $= Nothing
  return $ CharQuad vao arrayBuffer firstPosIndex 6

charBGQuad :: IO CharQuad
charBGQuad = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  arrayBuffer <- GL.genObjectName
  GL.bindBuffer ArrayBuffer $= Just arrayBuffer
  let vertexSize = sizeOf (0 :: GLfloat)
      firstPosIndex = 0
      vPosition = AttribLocation 0
      numVertices = 6 * 2
      size = fromIntegral (numVertices * vertexSize)
      stride = 0
  GL.bufferData ArrayBuffer $= (size, nullPtr, DynamicDraw)
  GL.vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstPosIndex))
  GL.vertexAttribArray vPosition $= Enabled
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer ArrayBuffer $= Nothing
  return $ CharQuad vao arrayBuffer firstPosIndex 6

drawCharQuad :: CharQuad -> IO ()
drawCharQuad (CharQuad arrayObject _ firstIndex numTriangles) = do
  GL.bindVertexArrayObject $= Just arrayObject
  GL.drawArrays Triangles firstIndex numTriangles

createTextRenderer ::
     Float
  -> Float
  -> Int
  -> Int
  -> Maybe FilePath
  -> Int
  -> Color4 GLfloat
  -> Color4 GLfloat
  -> IO TextRenderer
createTextRenderer front back width height fontPath charSize textColour bgColour = do
  cq <- charQuad
  cbq <- charBGQuad
  tprogram <-
    loadShaders
      [ ShaderInfo VertexShader (FileSource "shaders/textrenderer.vert")
      , ShaderInfo FragmentShader (FileSource "shaders/textrenderer.frag")
      ]
  bgshaderprogram <-
    loadShaders
      [ ShaderInfo VertexShader (FileSource "shaders/textrenderer-bg.vert")
      , ShaderInfo FragmentShader (FileSource "shaders/textrenderer-bg.frag")
      ]
  characters <- loadFontCharMap fontPath charSize
  let projectionMatrix =
        textCoordMatrix
          0
          (fromIntegral width)
          0
          (fromIntegral height)
          front
          back
  buffer <- createSavebuffer (fromIntegral width) (fromIntegral height)
  return $
    TextRenderer
      characters
      charSize
      projectionMatrix
      tprogram
      bgshaderprogram
      cq
      cbq
      textColour
      bgColour
      buffer

resizeTextRendererScreen ::
     Float -> Float -> Int -> Int -> TextRenderer -> IO TextRenderer
resizeTextRendererScreen front back width height trender =
  let projectionMatrix =
        textCoordMatrix
          0
          (fromIntegral width)
          0
          (fromIntegral height)
          front
          back
  in do deleteSavebuffer $ outbuffer trender
        nbuffer <- createSavebuffer (fromIntegral width) (fromIntegral height)
        return trender {pMatrix = projectionMatrix, outbuffer = nbuffer}

changeTextColour :: Color4 GLfloat -> TextRenderer -> TextRenderer
changeTextColour newColour trender = trender {textColour = newColour}

renderText :: Int -> Int -> TextRenderer -> String -> IO ()
renderText xpos ypos renderer strings = do
  let (Savebuffer fbo _ _ _ _) = outbuffer renderer
  GL.bindFramebuffer Framebuffer $= fbo
  renderCharacters xpos ypos renderer strings
  printErrors

renderTextbuffer :: TextRenderer -> IO ()
renderTextbuffer renderer = do
  GL.bindFramebuffer Framebuffer $= GL.defaultFramebufferObject
  let (Savebuffer _ text _ program quadVBO) = outbuffer renderer
  GL.currentProgram $= Just program
  GL.activeTexture $= TextureUnit 0
  GL.textureBinding Texture2D $= Just text
  drawQuadVBO quadVBO

renderCharacters :: Int -> Int -> TextRenderer -> String -> IO ()
renderCharacters xpos ypos renderer strings = do
  GL.blend $= Enabled
  GL.blendEquationSeparate $= (FuncAdd, FuncAdd)
  GL.blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
  GL.depthFunc $= Nothing
  GL.clearColor $= Color4 0.0 0.0 0.0 0.0
  GL.clear [ColorBuffer]
  let font = textFont renderer
  foldM_
    (\(xp, yp) c ->
       case c of
         '\n' -> return (xpos, yp + fontHeight font)
         _ ->
           maybe
             (return (xp, yp + fontAdvance font))
             (\c -> renderChar c xp yp font)
             (getCharacter font c))
    (xpos, ypos)
    strings
  where
    renderChar char xp yp f = do
      renderCharacterBackground renderer char xp yp f
      renderCharacter renderer char xp yp f

renderCharacter ::
     TextRenderer -> Character -> Int -> Int -> Font -> IO ((Int, Int))
renderCharacter renderer (Character c width height adv xBearing yBearing text) x y font =
  let baseline = fromIntegral (y + fontAscender font)
      gX1 = fromIntegral (x + xBearing)
      gX2 = gX1 + fromIntegral width
      gY1 = baseline - fromIntegral yBearing
      gY2 = gY1 + fromIntegral height
      charVerts =
        [ gX1
        , gY1
        , 0.0
        , 0.0 -- coord 1
        , gX1
        , gY2
        , 0.0
        , 1.0 -- coord 2
        , gX2
        , gY1
        , 1.0
        , 0.0 -- coord 3
        , gX1
        , gY2
        , 0.0
        , 1.0 -- coord 4
        , gX2
        , gY2
        , 1.0
        , 1.0 -- coord 5
        , gX2
        , gY1
        , 1.0
        , 0.0 -- coord 6
        ] :: [GLfloat]
      vertSize = sizeOf (head charVerts)
      numVerts = length charVerts
      size = fromIntegral (numVerts * vertSize)
      (CharQuad arrayObject arrayBuffer firstIndex numTriangles) =
        characterQuad renderer
  in do GL.currentProgram $= Just (textprogram renderer)
        GL.activeTexture $= TextureUnit 0
        GL.textureBinding Texture2D $= Just text
        GL.bindVertexArrayObject $= Just arrayObject
        GL.bindBuffer ArrayBuffer $= Just arrayBuffer
        textColourU <-
          GL.get $ GL.uniformLocation (textprogram renderer) "textColor"
        GL.uniform textColourU $= textColour renderer
        textBGColourU <-
          GL.get $ GL.uniformLocation (textprogram renderer) "textBGColor"
        GL.uniform textBGColourU $= textBGColour renderer
        (UniformLocation projU) <-
          GL.get $ GL.uniformLocation (textprogram renderer) "projection"
        with (pMatrix renderer) $
          GLRaw.glUniformMatrix4fv projU 1 (fromBool True) . castPtr
        withArray charVerts $ \ptr ->
          GL.bufferSubData ArrayBuffer WriteToBuffer 0 size ptr
        GL.drawArrays Triangles firstIndex numTriangles
        printErrors
        return (x + adv, y)

renderCharacterBackground ::
     TextRenderer -> Character -> Int -> Int -> Font -> IO ()
renderCharacterBackground renderer (Character _ _ _ adv _ _ _) x y font =
  let x1 = fromIntegral x
      x2 = fromIntegral $ x + adv
      y1 = fromIntegral y
      y2 = fromIntegral $ y + fontHeight font
      charVerts = [x1, y1, x1, y2, x2, y1, x1, y2, x2, y2, x2, y1] :: [GLfloat]
      vertSize = sizeOf (head charVerts)
      numVerts = length charVerts
      size = fromIntegral (numVerts * vertSize)
      (CharQuad arrayObject arrayBuffer firstIndex numTriangles) =
        characterBGQuad renderer
  in do GL.currentProgram $= Just (bgprogram renderer)
        GL.bindVertexArrayObject $= Just arrayObject
        GL.bindBuffer ArrayBuffer $= Just arrayBuffer
        textBGColourU <-
          GL.get $ GL.uniformLocation (bgprogram renderer) "textBGColor"
        GL.uniform textBGColourU $= textBGColour renderer
        (UniformLocation projU) <-
          GL.get $ GL.uniformLocation (bgprogram renderer) "projection"
        with (pMatrix renderer) $
          GLRaw.glUniformMatrix4fv projU 1 (fromBool True) . castPtr
        withArray charVerts $ \ptr ->
          GL.bufferSubData ArrayBuffer WriteToBuffer 0 size ptr
        GL.drawArrays Triangles firstIndex numTriangles
        printErrors
