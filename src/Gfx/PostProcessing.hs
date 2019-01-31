{-# LANGUAGE TemplateHaskell #-}

module Gfx.PostProcessing
  ( createPostProcessing
  , renderPostProcessing
  , usePostProcessing
  , deletePostProcessing
  , createTextDisplaybuffer
  , deleteSavebuffer
  , PostProcessing(..)
  , Savebuffer(..)
  , AnimationStyle(..)
  ) where

import           Graphics.Rendering.OpenGL as GL

import           Foreign.Marshal.Array     (withArray)
import           Foreign.Ptr               (nullPtr)
import           Foreign.Storable          (sizeOf)

import           Data.FileEmbed            (embedFile)
import           Gfx.LoadShaders           (ShaderInfo (..), ShaderSource (..),
                                            loadShaders)
import           Gfx.VertexBuffers         (VBO, createVBO, deleteVBO, drawVBO,
                                            setAttribPointer)

data AnimationStyle
  = NormalStyle
  | MotionBlur
  | PaintOver
  deriving (Eq, Show)

data PostProcessing = PostProcessing
  { input      :: Savebuffer
  , motionBlur :: Mixbuffer
  , paintOver  :: Mixbuffer
  , output     :: Savebuffer
  }

instance Show PostProcessing where
  show _ = "PostProcessing"

-- Simple Framebuffer with a texture that can be rendered to and then drawn out to a quad
data Savebuffer =
  Savebuffer FramebufferObject
             TextureObject
             TextureObject
             Program
             VBO

instance Show Savebuffer where
  show _ = "Savebuffer"

data Mixbuffer =
  Mixbuffer FramebufferObject
            TextureObject
            TextureObject
            Program
            VBO

-- 2D positions and texture coordinates
ordinaryQuadVertices :: [GLfloat]
ordinaryQuadVertices =
  [ -1
  , 1
  , 0
  , 1 -- v1
  , -1
  , -1
  , 0
  , 0 -- v2
  , 1
  , 1
  , 1
  , 1 -- v3
  , 1
  , 1
  , 1
  , 1 -- v4
  , -1
  , -1
  , 0
  , 0 -- v5
  , 1
  , -1
  , 1
  , 0 -- v6
  ]

textQuadVertices :: [GLfloat]
textQuadVertices =
  [ -1
  , 1
  , 0
  , 0 -- v1
  , -1
  , -1
  , 0
  , 1 -- v2
  , 1
  , 1
  , 1
  , 0 -- v3
  , 1
  , 1
  , 1
  , 0 -- v4
  , -1
  , -1
  , 0
  , 1 -- v5
  , 1
  , -1
  , 1
  , 1 -- v6
  ]

createQuadVBO :: [GLfloat] -> IO VBO
createQuadVBO quadVertices =
  let vertexSize = fromIntegral $ sizeOf (head quadVertices)
      posVSize = 2
      texVSize = 2
      firstPosIndex = 0
      firstTexIndex = posVSize * vertexSize
      vPosition = AttribLocation 0
      vTexCoord = AttribLocation 1
      numVertices = fromIntegral $ length quadVertices
      size = fromIntegral (numVertices * vertexSize)
      stride = fromIntegral ((posVSize + texVSize) * vertexSize)
      numArrIdx = 6
      quadConfig = do
        withArray quadVertices $ \ptr ->
          GL.bufferData ArrayBuffer $= (size, ptr, StaticDraw)
        setAttribPointer vPosition posVSize stride firstPosIndex
        setAttribPointer vTexCoord texVSize stride firstTexIndex
   in createVBO [quadConfig] Triangles firstPosIndex numArrIdx

create2DTexture :: GLint -> GLint -> IO TextureObject
create2DTexture width height = do
  text <- genObjectName
  GL.textureBinding GL.Texture2D $= Just text
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  let pd = PixelData RGBA UnsignedByte nullPtr
  GL.texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D width height) 0 pd
  GL.textureBinding Texture2D $= Nothing
  return text

createDepthbuffer :: GLint -> GLint -> IO TextureObject
createDepthbuffer width height = do
  depth <- genObjectName
  GL.textureBinding Texture2D $= Just depth
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  let pd = PixelData DepthComponent UnsignedByte nullPtr
  GL.texImage2D
    Texture2D
    NoProxy
    0
    DepthComponent24
    (TextureSize2D width height)
    0
    pd
  GL.framebufferTexture2D Framebuffer DepthAttachment Texture2D depth 0
  GL.textureBinding Texture2D $= Nothing
  return depth

createPostProcessing :: Int -> Int -> IO PostProcessing
createPostProcessing w h =
  let width = fromIntegral w
      height = fromIntegral h
   in do inputBuffer <- createSavebuffer width height
         motionBlurBuffer <- createMotionBlurbuffer width height
         paintOverBuffer <- createPaintOverbuffer width height
         outputBuffer <- createSavebuffer width height
         return $
           PostProcessing
             inputBuffer
             motionBlurBuffer
             paintOverBuffer
             outputBuffer

deletePostProcessing :: PostProcessing -> IO ()
deletePostProcessing post = do
  deleteSavebuffer $ input post
  deleteMixbuffer $ motionBlur post
  deleteMixbuffer $ paintOver post
  deleteSavebuffer $ output post

createSavebuffer :: GLint -> GLint -> IO Savebuffer
createSavebuffer width height = do
  fbo <- genObjectName
  GL.bindFramebuffer Framebuffer $= fbo
  text <- create2DTexture width height
  GL.framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D text 0
  depth <- createDepthbuffer width height
  qvbo <- createQuadVBO ordinaryQuadVertices
  program <-
    loadShaders
      [ ShaderInfo
          VertexShader
          (ByteStringSource $(embedFile "src/assets/shaders/savebuffer.vert"))
      , ShaderInfo
          FragmentShader
          (ByteStringSource $(embedFile "src/assets/shaders/savebuffer.frag"))
      ]
  return $ Savebuffer fbo text depth program qvbo

createTextDisplaybuffer :: GLint -> GLint -> IO Savebuffer
createTextDisplaybuffer width height = do
  fbo <- genObjectName
  GL.bindFramebuffer Framebuffer $= fbo
  text <- create2DTexture width height
  GL.framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D text 0
  depth <- createDepthbuffer width height
  qvbo <- createQuadVBO textQuadVertices
  program <-
    loadShaders
      [ ShaderInfo
          VertexShader
          (ByteStringSource $(embedFile "src/assets/shaders/savebuffer.vert"))
      , ShaderInfo
          FragmentShader
          (ByteStringSource $(embedFile "src/assets/shaders/savebuffer.frag"))
      ]
  return $ Savebuffer fbo text depth program qvbo

deleteSavebuffer :: Savebuffer -> IO ()
deleteSavebuffer (Savebuffer sbfbo sbtext sbdepth sbprogram sbvbo) = do
  deleteObjectName sbtext
  deleteObjectName sbdepth
  deleteObjectName sbprogram
  deleteVBO sbvbo
  deleteObjectName sbfbo

createMotionBlurbuffer :: GLint -> GLint -> IO Mixbuffer
createMotionBlurbuffer width height = do
  fbo <- genObjectName
  bindFramebuffer Framebuffer $= fbo
  text <- create2DTexture width height
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D text 0
  depth <- createDepthbuffer width height
  qvbo <- createQuadVBO ordinaryQuadVertices
  program <-
    loadShaders
      [ ShaderInfo
          VertexShader
          (ByteStringSource $(embedFile "src/assets/shaders/motionBlur.vert"))
      , ShaderInfo
          FragmentShader
          (ByteStringSource $(embedFile "src/assets/shaders/motionBlur.frag"))
      ]
  return $ Mixbuffer fbo text depth program qvbo

deleteMixbuffer :: Mixbuffer -> IO ()
deleteMixbuffer (Mixbuffer mfbo mtext depth mprogram mbvbo) = do
  deleteObjectName mtext
  deleteObjectName depth
  deleteObjectName mprogram
  deleteVBO mbvbo
  deleteObjectName mfbo

createPaintOverbuffer :: GLint -> GLint -> IO Mixbuffer
createPaintOverbuffer width height = do
  fbo <- genObjectName
  bindFramebuffer Framebuffer $= fbo
  text <- create2DTexture width height
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D text 0
  depth <- createDepthbuffer width height
  qvbo <- createQuadVBO ordinaryQuadVertices
  program <-
    loadShaders
      [ ShaderInfo
          VertexShader
          (ByteStringSource $(embedFile "src/assets/shaders/paintOver.vert"))
      , ShaderInfo
          FragmentShader
          (ByteStringSource $(embedFile "src/assets/shaders/paintOver.frag"))
      ]
  return $ Mixbuffer fbo text depth program qvbo

usePostProcessing :: PostProcessing -> IO ()
usePostProcessing post = do
  let (Savebuffer fbo _ _ _ _) = input post
  bindFramebuffer Framebuffer $= fbo

renderPostProcessing :: PostProcessing -> AnimationStyle -> IO ()
renderPostProcessing post animStyle = do
  depthFunc $= Nothing
  let outbuffer@(Savebuffer outFBO previousFrame _ _ _) = output post
  bindFramebuffer Framebuffer $= outFBO
  case animStyle of
    NormalStyle -> renderSavebuffer $ input post
    MotionBlur -> do
      let (Savebuffer _ sceneFrame sceneDepth _ _) = input post
      renderMotionBlurbuffer (motionBlur post) sceneFrame previousFrame 0.7
    PaintOver -> do
      let (Savebuffer _ sceneFrame sceneDepth _ _) = input post
      renderPaintOverbuffer (paintOver post) sceneDepth sceneFrame previousFrame
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  renderSavebuffer outbuffer

renderSavebuffer :: Savebuffer -> IO ()
renderSavebuffer (Savebuffer _ text _ program quadVBO) = do
  currentProgram $= Just program
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just text
  drawVBO quadVBO

renderMotionBlurbuffer ::
     Mixbuffer -> TextureObject -> TextureObject -> GLfloat -> IO ()
renderMotionBlurbuffer (Mixbuffer _ _ _ program quadVBO) nextFrame lastFrame mix = do
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just nextFrame
  activeTexture $= TextureUnit 1
  textureBinding Texture2D $= Just lastFrame
  currentProgram $= Just program
  texFramebufferU <- GL.get $ uniformLocation program "texFramebuffer"
  lastFrameU <- GL.get $ uniformLocation program "lastFrame"
  mixRatioU <- GL.get $ uniformLocation program "mixRatio"
  uniform texFramebufferU $= TextureUnit 0
  uniform lastFrameU $= TextureUnit 1
  uniform mixRatioU $= mix
  drawVBO quadVBO

renderPaintOverbuffer ::
     Mixbuffer -> TextureObject -> TextureObject -> TextureObject -> IO ()
renderPaintOverbuffer (Mixbuffer _ _ _ program quadVBO) depth nextFrame lastFrame = do
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just nextFrame
  activeTexture $= TextureUnit 1
  textureBinding Texture2D $= Just lastFrame
  activeTexture $= TextureUnit 2
  textureBinding Texture2D $= Just depth
  currentProgram $= Just program
  texFramebufferU <- GL.get $ uniformLocation program "texFramebuffer"
  lastFrameU <- GL.get $ uniformLocation program "lastFrame"
  depthU <- GL.get $ uniformLocation program "depth"
  uniform texFramebufferU $= TextureUnit 0
  uniform lastFrameU $= TextureUnit 1
  uniform depthU $= TextureUnit 2
  drawVBO quadVBO
