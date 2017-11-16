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

import           Control.Monad

import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

import qualified Data.Map.Strict           as M

import           Gfx.FreeType              (Character (..), CharacterMap,
                                            loadFontCharMap)
import           Gfx.GeometryBuffers       (bufferOffset)
import           Gfx.LoadShaders
import qualified Graphics.GL               as GLRaw
import           Graphics.Rendering.OpenGL as GL

import           Gfx.PostProcessing        (Savebuffer (..), createSavebuffer,
                                            deleteSavebuffer, drawQuadVBO)

import           Data.Vec                  (Mat44, multmm)
import           Gfx.Matrices

import           ErrorHandling             (printErrors)

data CharQuad =
  CharQuad VertexArrayObject
           BufferObject
           ArrayIndex
           NumArrayIndices
  deriving (Show, Eq)

data TextRenderer = TextRenderer
  { characterMap    :: CharacterMap
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

-- TODO - better error handling
getCharacter :: TextRenderer -> Char -> Character
getCharacter tr c = characterMap tr M.! c

charQuad :: IO CharQuad
charQuad = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  let vertexSize = sizeOf (0 :: GLfloat)
      firstPosIndex = 0
      firstTexIndex = 2 * vertexSize
      vPosition = AttribLocation 0
      vTexCoord = AttribLocation 1
      numVertices = 6 * 4
      size = fromIntegral (numVertices * vertexSize)
      stride = fromIntegral (4 * vertexSize)
  bufferData ArrayBuffer $= (size, nullPtr, DynamicDraw)
  vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstPosIndex))
  vertexAttribPointer vTexCoord $=
    (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstTexIndex))
  vertexAttribArray vPosition $= Enabled
  vertexAttribArray vTexCoord $= Enabled
  bindVertexArrayObject $= Nothing
  bindBuffer ArrayBuffer $= Nothing
  return $ CharQuad vao arrayBuffer firstPosIndex 6

charBGQuad :: IO CharQuad
charBGQuad = do
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  let vertexSize = sizeOf (0 :: GLfloat)
      firstPosIndex = 0
      vPosition = AttribLocation 0
      numVertices = 6 * 2
      size = fromIntegral (numVertices * vertexSize)
      stride = 0
  bufferData ArrayBuffer $= (size, nullPtr, DynamicDraw)
  vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset firstPosIndex))
  vertexAttribArray vPosition $= Enabled
  bindVertexArrayObject $= Nothing
  bindBuffer ArrayBuffer $= Nothing
  return $ CharQuad vao arrayBuffer firstPosIndex 6

drawCharQuad :: CharQuad -> IO ()
drawCharQuad (CharQuad arrayObject _ firstIndex numTriangles) = do
  bindVertexArrayObject $= Just arrayObject
  drawArrays Triangles firstIndex numTriangles

createTextRenderer ::
     Float
  -> Float
  -> Int
  -> Int
  -> String
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
  bindFramebuffer Framebuffer $= fbo
  renderCharacters xpos ypos renderer strings
  printErrors

renderTextbuffer :: TextRenderer -> IO ()
renderTextbuffer renderer = do
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  let (Savebuffer _ text _ program quadVBO) = outbuffer renderer
  currentProgram $= Just program
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just text
  drawQuadVBO quadVBO

renderCharacters :: Int -> Int -> TextRenderer -> String -> IO ()
renderCharacters xpos ypos renderer strings = do
  blend $= Enabled
  blendEquationSeparate $= (FuncAdd, FuncAdd)
  blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
  depthFunc $= Nothing
  clearColor $= Color4 0.0 0.0 0.0 0.0
  clear [ColorBuffer]
  foldM_
    (\(xp, yp) c ->
       case c of
         '\n' -> return (xpos, yp + charSize renderer)
         _ -> do
           let char@(Character _ _ _ adv _) = getCharacter renderer c
           renderCharacterBackground renderer char xp yp
           renderCharacter renderer char xp yp
           return (xp + adv, yp))
    (xpos, ypos)
    strings

renderCharacter :: TextRenderer -> Character -> Int -> Int -> IO ()
renderCharacter renderer (Character c width height adv text) x y =
  let xPos = fromIntegral x
      yPos = fromIntegral y
      lbottom = yPos + fromIntegral (charSize renderer) :: GLfloat
      ltop = lbottom - fromIntegral height :: GLfloat
      w = fromIntegral width
      h = fromIntegral height
      top = yPos + fromIntegral (charSize renderer) - fromIntegral height
      charVerts =
        [ xPos
        , ltop
        , 0.0
        , 0.0
        , xPos
        , lbottom
        , 0.0
        , 1.0
        , xPos + w
        , ltop
        , 1.0
        , 0.0
        , xPos
        , lbottom
        , 0.0
        , 1.0
        , xPos + w
        , lbottom
        , 1.0
        , 1.0
        , xPos + w
        , ltop
        , 1.0
        , 0.0
        ] :: [GLfloat]
      vertSize = sizeOf (head charVerts)
      numVerts = length charVerts
      size = fromIntegral (numVerts * vertSize)
      (CharQuad arrayObject arrayBuffer firstIndex numTriangles) =
        characterQuad renderer
  in do currentProgram $= Just (textprogram renderer)
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just text
        bindVertexArrayObject $= Just arrayObject
        bindBuffer ArrayBuffer $= Just arrayBuffer
        textColourU <-
          GL.get $ uniformLocation (textprogram renderer) "textColor"
        uniform textColourU $= textColour renderer
        textBGColourU <-
          GL.get $ uniformLocation (textprogram renderer) "textBGColor"
        uniform textBGColourU $= textBGColour renderer
        (UniformLocation projU) <-
          GL.get $ uniformLocation (textprogram renderer) "projection"
        with (pMatrix renderer) $
          GLRaw.glUniformMatrix4fv projU 1 (fromBool True) . castPtr
        withArray charVerts $ \ptr ->
          bufferSubData ArrayBuffer WriteToBuffer 0 size ptr
        drawArrays Triangles firstIndex numTriangles
        printErrors

renderCharacterBackground :: TextRenderer -> Character -> Int -> Int -> IO ()
renderCharacterBackground renderer (Character c width height adv text) x y =
  let xPos = fromIntegral x
      yPos = fromIntegral y
      w = fromIntegral adv
      h = fromIntegral $ charSize renderer
      charVerts =
        [ xPos
        , yPos
        , xPos
        , yPos + h
        , xPos + w
        , yPos
        , xPos
        , yPos + h
        , xPos + w
        , yPos + h
        , xPos + w
        , yPos
        ] :: [GLfloat]
      vertSize = sizeOf (head charVerts)
      numVerts = length charVerts
      size = fromIntegral (numVerts * vertSize)
      (CharQuad arrayObject arrayBuffer firstIndex numTriangles) =
        characterBGQuad renderer
  in do currentProgram $= Just (bgprogram renderer)
        bindVertexArrayObject $= Just arrayObject
        bindBuffer ArrayBuffer $= Just arrayBuffer
        textBGColourU <-
          GL.get $ uniformLocation (bgprogram renderer) "textBGColor"
        uniform textBGColourU $= textBGColour renderer
        (UniformLocation projU) <-
          GL.get $ uniformLocation (bgprogram renderer) "projection"
        with (pMatrix renderer) $
          GLRaw.glUniformMatrix4fv projU 1 (fromBool True) . castPtr
        withArray charVerts $ \ptr ->
          bufferSubData ArrayBuffer WriteToBuffer 0 size ptr
        drawArrays Triangles firstIndex numTriangles
        printErrors
