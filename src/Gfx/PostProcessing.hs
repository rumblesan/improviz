{-# LANGUAGE TemplateHaskell #-}

module Gfx.PostProcessing
  ( createPostProcessing
  , renderPostProcessing
  , usePostProcessing
  , deletePostProcessing
  , createTextDisplaybuffer
  , deleteSavebuffer
  , PostProcessingBuffers(..)
  , Savebuffer(..)
  , AnimationStyle(..)
  ) where

import           Control.Monad.State.Strict
import           Graphics.Rendering.OpenGL     as GL
import           Lens.Simple                    ( (^.)
                                                , makeLenses
                                                , use
                                                )

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

type PostProcessing v = StateT PostProcessingBuffers IO v

runPostProcessing :: PostProcessingBuffers -> PostProcessing v -> IO v
runPostProcessing post f = evalStateT f post

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

data PostProcessingBuffers = PostProcessingBuffers
  { _input      :: Savebuffer
  , _motionBlur :: Processingbuffer
  , _paintOver  :: Processingbuffer
  , _output     :: Savebuffer
  }

makeLenses ''PostProcessingBuffers

instance Show PostProcessingBuffers where
  show _ = "PostProcessingBuffers"

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

createPostProcessing :: Int -> Int -> IO PostProcessingBuffers
createPostProcessing w h =
  let width  = fromIntegral w
      height = fromIntegral h
  in  do
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
        return $ PostProcessingBuffers inputBuffer
                                       motionBlurBuffer
                                       paintOverBuffer
                                       outputBuffer

deletePostProcessing :: PostProcessingBuffers -> IO ()
deletePostProcessing post = do
  deleteSavebuffer $ post ^. input
  deleteProcessingbuffer $ post ^. motionBlur
  deleteProcessingbuffer $ post ^. paintOver
  deleteSavebuffer $ post ^. output

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

usePostProcessing :: PostProcessingBuffers -> IO ()
usePostProcessing post = runPostProcessing post usePostProcessingST

usePostProcessingST :: PostProcessing ()
usePostProcessingST = do
  (Savebuffer fbo _ _ _ _) <- use input
  liftIO $ bindFramebuffer Framebuffer $= fbo

renderPostProcessing :: PostProcessingBuffers -> AnimationStyle -> IO ()
renderPostProcessing post animStyle =
  runPostProcessing post (renderPostProcessingST animStyle)

renderPostProcessingST :: AnimationStyle -> PostProcessing ()
renderPostProcessingST animStyle = do
  outbuffer@(Savebuffer outFBO previousFrame _ _ _) <- use output
  liftIO $ do
    depthFunc $= Nothing
    bindFramebuffer Framebuffer $= outFBO
  case animStyle of
    NormalStyle -> use input >>= renderSavebuffer
    MotionBlur  -> use motionBlur >>= renderPostProcessingBuffer
    PaintOver   -> use paintOver >>= renderPostProcessingBuffer
  liftIO (bindFramebuffer Framebuffer $= defaultFramebufferObject)
  renderSavebuffer outbuffer

renderSavebuffer :: Savebuffer -> PostProcessing ()
renderSavebuffer (Savebuffer _ text _ program quadVBO) = liftIO $ do
  currentProgram $= Just program
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just text
  drawVBO quadVBO

setUniform :: (String, GL.VariableType, UniformLocation) -> PostProcessing ()
setUniform ("texFramebuffer", _, uniformLoc) = do
  (Savebuffer _ sceneFrame _ _ _) <- use input
  liftIO $ do
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just sceneFrame
    uniform uniformLoc $= TextureUnit 0
setUniform ("lastFrame", _, uniformLoc) = do
  (Savebuffer _ lastFrame _ _ _) <- use output
  liftIO $ do
    activeTexture $= TextureUnit 1
    textureBinding Texture2D $= Just lastFrame
    uniform uniformLoc $= TextureUnit 1
setUniform ("depth", _, uniformLoc) = do
  (Savebuffer _ _ depth _ _) <- use input
  liftIO $ do
    activeTexture $= TextureUnit 2
    textureBinding Texture2D $= Just depth
    uniform uniformLoc $= TextureUnit 2
setUniform ("mixRatio", _, uniformLoc) =
  let mix = 0.7 :: GLfloat in liftIO (uniform uniformLoc $= mix)
setUniform (name, _, _) = liftIO $ logError $ name ++ " is not a known uniform"

renderPostProcessingBuffer :: Processingbuffer -> PostProcessing ()
renderPostProcessingBuffer (Processingbuffer _ _ _ shader quadVBO) = do
  liftIO (currentProgram $= (Just $ ppProgram shader))
  mapM_ setUniform (ppUniforms shader)
  liftIO $ drawVBO quadVBO

