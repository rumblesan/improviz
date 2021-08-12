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

import           Graphics.Rendering.OpenGL     as GL

import           Foreign.Marshal.Array          ( withArray )
import           Foreign.Ptr                    ( nullPtr )
import           Foreign.Storable               ( sizeOf )

import           Data.FileEmbed                 ( embedFile )
import           Gfx.LoadShaders                ( ShaderInfo(..)
                                                , ShaderSource(..)
                                                , loadShaders
                                                )
import           Gfx.Shaders                    ( getAttribLoc
                                                , getUniformLoc
                                                )
import           Gfx.VertexBuffers              ( VBO
                                                , createVBO
                                                , deleteVBO
                                                , drawVBO
                                                , setAttribPointer
                                                )
import           Logging                        ( logError
                                                , logInfo
                                                )

data AnimationStyle
  = NormalStyle
  | MotionBlur
  | PaintOver
  deriving (Eq, Show)

data PostProcessing = PostProcessing
  { input      :: Savebuffer
  , motionBlur :: Processingbuffer
  , paintOver  :: Processingbuffer
  , output     :: Savebuffer
  }

instance Show PostProcessing where
  show _ = "PostProcessing"

-- Simple Framebuffer with a texture that can be rendered to and then drawn out to a quad
data Savebuffer = Savebuffer FramebufferObject
                             TextureObject
                             TextureObject
                             Program
                             VBO

instance Show Savebuffer where
  show _ = "Savebuffer"

data Processingbuffer = Processingbuffer FramebufferObject
                                         TextureObject
                                         TextureObject
                                         PostProcessingShader
                                         VBO

data PostProcessingShader = PostProcessingShader
  { ppName       :: String
  , ppProgram    :: GL.Program
  , ppUniforms   :: [(String, GL.VariableType, GL.UniformLocation)]
  , ppAttributes :: [(String, GL.VariableType, GL.AttribLocation)]
  }
  deriving (Show, Eq)

loadPostProcessingShader
  :: String -> ShaderSource -> ShaderSource -> IO PostProcessingShader
loadPostProcessingShader name vertShader fragShader = do
  program <- loadShaders
    [ ShaderInfo GL.VertexShader   vertShader
    , ShaderInfo GL.FragmentShader fragShader
    ]
  GL.currentProgram $= Just program
  uniformInfo <- GL.get $ GL.activeUniforms program
  uniforms    <- mapM (getUniformLoc program) uniformInfo
  attribInfo  <- GL.get $ GL.activeAttribs program
  attributes  <- mapM (getAttribLoc program) attribInfo
  logInfo $ "Loading " ++ name ++ " post processor"
  return $ PostProcessingShader name program uniforms attributes

-- 2D positions and texture coordinates
-- brittany-disable-next-binding
ordinaryQuadVertices :: [GLfloat]
ordinaryQuadVertices = [ -1,  1, 0, 1 -- v1
                       ,  1,  1, 1, 1 -- v2
                       , -1, -1, 0, 0 -- v3
                       , -1, -1, 0, 0 -- v4
                       ,  1,  1, 1, 1 -- v5
                       ,  1, -1, 1, 0 -- v6
                       ]

-- brittany-disable-next-binding
textQuadVertices :: [GLfloat]
textQuadVertices = [ -1,  1, 0, 0 -- v1
                   ,  1,  1, 1, 0 -- v2
                   , -1, -1, 0, 1 -- v3
                   , -1, -1, 0, 1 -- v4
                   ,  1,  1, 1, 0 -- v5
                   ,  1, -1, 1, 1 -- v6
                   ]

createQuadVBO :: [GLfloat] -> IO VBO
createQuadVBO quadVertices =
  let vertexSize    = fromIntegral $ sizeOf (head quadVertices)
      posVSize      = 2
      texVSize      = 2
      firstPosIndex = 0
      firstTexIndex = posVSize * vertexSize
      vPosition     = AttribLocation 0
      vTexCoord     = AttribLocation 1
      numVertices   = fromIntegral $ length quadVertices
      size          = fromIntegral (numVertices * vertexSize)
      stride        = fromIntegral ((posVSize + texVSize) * vertexSize)
      numArrIdx     = 6
      quadConfig    = do
        withArray quadVertices
          $ \ptr -> GL.bufferData ArrayBuffer $= (size, ptr, StaticDraw)
        setAttribPointer vPosition posVSize stride firstPosIndex
        setAttribPointer vTexCoord texVSize stride firstTexIndex
  in  createVBO [quadConfig] Triangles firstPosIndex numArrIdx

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
  GL.texImage2D Texture2D
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
  let
    width  = fromIntegral w
    height = fromIntegral h
  in
    do
      inputBuffer      <- createSavebuffer width height
      motionBlurBuffer <- createPostProcessingBuffer
        width
        height
        (ByteStringSource $(embedFile "src/assets/shaders/motionBlur.vert"))
        (ByteStringSource $(embedFile "src/assets/shaders/motionBlur.frag"))
        ordinaryQuadVertices
      paintOverBuffer <- createPostProcessingBuffer
        width
        height
        (ByteStringSource $(embedFile "src/assets/shaders/paintOver.vert"))
        (ByteStringSource $(embedFile "src/assets/shaders/paintOver.frag"))
        ordinaryQuadVertices
      outputBuffer <- createSavebuffer width height
      return $ PostProcessing inputBuffer
                              motionBlurBuffer
                              paintOverBuffer
                              outputBuffer

deletePostProcessing :: PostProcessing -> IO ()
deletePostProcessing post = do
  deleteSavebuffer $ input post
  deleteProcessingbuffer $ motionBlur post
  deleteProcessingbuffer $ paintOver post
  deleteSavebuffer $ output post

createSavebuffer :: GLint -> GLint -> IO Savebuffer
createSavebuffer width height = do
  fbo <- genObjectName
  GL.bindFramebuffer Framebuffer $= fbo
  text <- create2DTexture width height
  GL.framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D text 0
  depth   <- createDepthbuffer width height
  qvbo    <- createQuadVBO ordinaryQuadVertices
  program <- loadShaders
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
  depth   <- createDepthbuffer width height
  qvbo    <- createQuadVBO textQuadVertices
  program <- loadShaders
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

deleteProcessingbuffer :: Processingbuffer -> IO ()
deleteProcessingbuffer (Processingbuffer mfbo mtext depth shader mbvbo) = do
  deleteObjectName mtext
  deleteObjectName depth
  deleteObjectName (ppProgram shader)
  deleteVBO mbvbo
  deleteObjectName mfbo

createPostProcessingBuffer
  :: GLint
  -> GLint
  -> ShaderSource
  -> ShaderSource
  -> [GLfloat]
  -> IO Processingbuffer
createPostProcessingBuffer width height vertShader fragShader vertices = do
  fbo <- genObjectName
  bindFramebuffer Framebuffer $= fbo
  text <- create2DTexture width height
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D text 0
  depth  <- createDepthbuffer width height
  qvbo   <- createQuadVBO vertices
  shader <- loadPostProcessingShader "" vertShader fragShader
  return $ Processingbuffer fbo text depth shader qvbo

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
    MotionBlur  -> do
      let (Savebuffer _ sceneFrame sceneDepth _ _) = input post
      renderPostProcessingBuffer (motionBlur post)
                                 sceneDepth
                                 sceneFrame
                                 previousFrame
                                 0.7
    PaintOver -> do
      let (Savebuffer _ sceneFrame sceneDepth _ _) = input post
      renderPostProcessingBuffer (paintOver post)
                                 sceneDepth
                                 sceneFrame
                                 previousFrame
                                 0.7
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  renderSavebuffer outbuffer

renderSavebuffer :: Savebuffer -> IO ()
renderSavebuffer (Savebuffer _ text _ program quadVBO) = do
  currentProgram $= Just program
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just text
  drawVBO quadVBO

setUniform
  :: TextureObject
  -> TextureObject
  -> TextureObject
  -> GLfloat
  -> (String, GL.VariableType, UniformLocation)
  -> IO ()
setUniform _ nextFrame _ _ ("texFramebuffer", _, uniformLoc) = do
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just nextFrame
  uniform uniformLoc $= TextureUnit 0
setUniform _ _ lastFrame _ ("lastFrame", _, uniformLoc) = do
  activeTexture $= TextureUnit 1
  textureBinding Texture2D $= Just lastFrame
  uniform uniformLoc $= TextureUnit 1
setUniform depth _ _ _ ("depth", _, uniformLoc) = do
  activeTexture $= TextureUnit 2
  textureBinding Texture2D $= Just depth
  uniform uniformLoc $= TextureUnit 2
setUniform _ _ _ mix ("mixRatio", _, uniformLoc) = do
  uniform uniformLoc $= mix
setUniform _ _ _ mix (name, _, _) =
  logError $ name ++ " is not a known uniform"

renderPostProcessingBuffer
  :: Processingbuffer
  -> TextureObject
  -> TextureObject
  -> TextureObject
  -> GLfloat
  -> IO ()
renderPostProcessingBuffer (Processingbuffer _ _ _ shader quadVBO) depth nextFrame lastFrame mix
  = do
    let program = ppProgram shader
    currentProgram $= Just program
    mapM_ (setUniform depth nextFrame lastFrame mix) (ppUniforms shader)
    drawVBO quadVBO

