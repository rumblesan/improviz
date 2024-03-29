{-# LANGUAGE TemplateHaskell #-}

module Gfx.PostProcessing
  ( createPostProcessing
  , renderPostProcessing
  , usePostProcessing
  , deletePostProcessing
  , createTextDisplaybuffer
  , deleteSavebuffer
  , filterVars
  , PostProcessingConfig(..)
  , Savebuffer(..)
  , AnimationStyle(..)
  ) where

import           Configuration.Shaders
import           Control.Monad.State.Strict
import           Data.Either                    ( partitionEithers )
import qualified Data.Map.Strict               as M
import           Graphics.Rendering.OpenGL     as GL
import           Lens.Simple                    ( (^.)
                                                , makeLenses
                                                , use
                                                , uses
                                                )
import qualified Util.SettingMap               as SM

import           Foreign.Marshal.Array          ( withArray )
import           Foreign.Ptr                    ( nullPtr )
import           Foreign.Storable               ( sizeOf )

import           Data.FileEmbed                 ( embedStringFile )
import           Gfx.LoadShaders                ( ShaderInfo(..)
                                                , ShaderSource(..)
                                                , loadShaders
                                                )
import           Gfx.OpenGL                     ( valueToUniform )
import           Gfx.Shaders                    ( getAttribLoc
                                                , getUniformLoc
                                                )
import           Gfx.VertexBuffers              ( VBO
                                                , createVBO
                                                , deleteVBO
                                                , drawVBO
                                                , setAttribPointer
                                                )
import           Language.Ast                   ( Value )
import           Logging                        ( logError
                                                , logInfo
                                                )

data AnimationStyle
  = NormalStyle
  | UserFilter String
  deriving (Eq, Show)

type PostProcessing v = StateT PostProcessingConfig IO v

runPostProcessing :: PostProcessingConfig -> PostProcessing v -> IO v
runPostProcessing post f = evalStateT f post

-- Simple Framebuffer with a texture that can be rendered to and then drawn out to a quad
data Savebuffer = Savebuffer FramebufferObject
                             TextureObject
                             TextureObject
                             Program
                             VBO

instance Show Savebuffer where
  show _ = "Savebuffer"

data PostFilter = PostFilter FramebufferObject
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

type FilterVars = SM.SettingMap String Value

data PostProcessingConfig = PostProcessingConfig
  { _input       :: Savebuffer
  , _output      :: Savebuffer
  , _userFilters :: M.Map String PostFilter
  , _filterVars  :: FilterVars
  }

makeLenses ''PostProcessingConfig

instance Show PostProcessingConfig where
  show _ = "PostProcessingConfig"

loadPostProcessingShader :: ShaderData -> IO PostProcessingShader
loadPostProcessingShader (ShaderData name vertShader fragShader) = do
  program <- loadShaders
    [ ShaderInfo GL.VertexShader   (StringSource vertShader)
    , ShaderInfo GL.FragmentShader (StringSource fragShader)
    ]
  GL.currentProgram $= Just program
  uniformInfo <- GL.get $ GL.activeUniforms program
  uniforms    <- mapM (getUniformLoc program) uniformInfo
  attribInfo  <- GL.get $ GL.activeAttribs program
  attributes  <- mapM (getAttribLoc program) attribInfo
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

createPostProcessing :: [FilePath] -> Int -> Int -> IO PostProcessingConfig
createPostProcessing filterDirectories w h =
  let width  = fromIntegral w
      height = fromIntegral h
  in  do
        inputBuffer      <- createSaveBuffer
          width
          height
          ordinaryQuadVertices
          $(embedStringFile "src/assets/shaders/savebuffer.vert")
          $(embedStringFile "src/assets/shaders/savebuffer.frag")
        outputBuffer <- createSaveBuffer
          width
          height
          ordinaryQuadVertices
          $(embedStringFile "src/assets/shaders/savebuffer.vert")
          $(embedStringFile "src/assets/shaders/savebuffer.frag")
        (loadedFilters, varDefaults) <- loadFilterDirectories filterDirectories
        userFilters <- mapM (createPostProcessingFilter width height ordinaryQuadVertices) loadedFilters
        return $ PostProcessingConfig inputBuffer outputBuffer userFilters varDefaults

deletePostProcessing :: PostProcessingConfig -> IO ()
deletePostProcessing post = do
  deleteSavebuffer $ post ^. input
  deleteSavebuffer $ post ^. output
  mapM_ deletePostFilter $ post ^. userFilters

createTextDisplaybuffer :: GLint -> GLint -> IO Savebuffer
createTextDisplaybuffer width height =
  createSaveBuffer
    width
    height
    textQuadVertices
    $(embedStringFile "src/assets/shaders/savebuffer.vert")
    $(embedStringFile "src/assets/shaders/savebuffer.frag")

createSaveBuffer
  :: GLint -> GLint -> [GLfloat] -> String -> String -> IO Savebuffer
createSaveBuffer width height vertices vertShader fragShader = do
  fbo <- genObjectName
  GL.bindFramebuffer Framebuffer $= fbo
  text <- create2DTexture width height
  GL.framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D text 0
  depth   <- createDepthbuffer width height
  qvbo    <- createQuadVBO vertices
  program <- loadShaders
    [ ShaderInfo VertexShader   (StringSource vertShader)
    , ShaderInfo FragmentShader (StringSource fragShader)
    ]
  return $ Savebuffer fbo text depth program qvbo

deleteSavebuffer :: Savebuffer -> IO ()
deleteSavebuffer (Savebuffer sbfbo sbtext sbdepth sbprogram sbvbo) = do
  deleteObjectName sbtext
  deleteObjectName sbdepth
  deleteObjectName sbprogram
  deleteVBO sbvbo
  deleteObjectName sbfbo

deletePostFilter :: PostFilter -> IO ()
deletePostFilter (PostFilter mfbo mtext depth shader mbvbo) = do
  deleteObjectName mtext
  deleteObjectName depth
  deleteObjectName (ppProgram shader)
  deleteVBO mbvbo
  deleteObjectName mfbo

createPostProcessingFilter
  :: GLint -> GLint -> [GLfloat] -> PostProcessingShader -> IO PostFilter
createPostProcessingFilter width height vertices shader = do
  fbo <- genObjectName
  bindFramebuffer Framebuffer $= fbo
  text <- create2DTexture width height
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D text 0
  depth <- createDepthbuffer width height
  qvbo  <- createQuadVBO vertices
  return $ PostFilter fbo text depth shader qvbo

usePostProcessing :: PostProcessingConfig -> IO ()
usePostProcessing post = runPostProcessing post usePostProcessingST

usePostProcessingST :: PostProcessing ()
usePostProcessingST = do
  (Savebuffer fbo _ _ _ _) <- use input
  liftIO $ bindFramebuffer Framebuffer $= fbo

renderPostProcessing
  :: PostProcessingConfig -> FilterVars -> AnimationStyle -> IO ()
renderPostProcessing post postVars animStyle =
  runPostProcessing post (renderPostProcessingST postVars animStyle)

renderPostProcessingST :: FilterVars -> AnimationStyle -> PostProcessing ()
renderPostProcessingST postVars animStyle = do
  outbuffer@(Savebuffer outFBO previousFrame _ _ _) <- use output
  liftIO $ do
    depthFunc $= Nothing
    bindFramebuffer Framebuffer $= outFBO
  case animStyle of
    NormalStyle     -> use input >>= renderSavebuffer
    UserFilter name -> do
      maybeFilter <- uses userFilters (M.lookup name)
      case maybeFilter of
        Nothing           -> use input >>= renderSavebuffer
        Just filterBuffer -> renderPostProcessingFilter postVars filterBuffer
  liftIO (bindFramebuffer Framebuffer $= defaultFramebufferObject)
  renderSavebuffer outbuffer

renderSavebuffer :: Savebuffer -> PostProcessing ()
renderSavebuffer (Savebuffer _ text _ program quadVBO) = liftIO $ do
  currentProgram $= Just program
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just text
  drawVBO quadVBO

setUniform
  :: FilterVars
  -> (String, GL.VariableType, UniformLocation)
  -> PostProcessing ()
setUniform _ ("texFramebuffer", _, uniformLoc) = do
  (Savebuffer _ sceneFrame _ _ _) <- use input
  liftIO $ do
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just sceneFrame
    uniform uniformLoc $= TextureUnit 0
setUniform _ ("lastFrame", _, uniformLoc) = do
  (Savebuffer _ lastFrame _ _ _) <- use output
  liftIO $ do
    activeTexture $= TextureUnit 1
    textureBinding Texture2D $= Just lastFrame
    uniform uniformLoc $= TextureUnit 1
setUniform _ ("depth", _, uniformLoc) = do
  (Savebuffer _ _ depth _ _) <- use input
  liftIO $ do
    activeTexture $= TextureUnit 2
    textureBinding Texture2D $= Just depth
    uniform uniformLoc $= TextureUnit 2
setUniform filterVars (name, uniformType, uniformLoc) = do
  let filtVar = filterVars ^. SM.value name
  liftIO $ case filtVar of
    Nothing -> logError $ name ++ " is not a known uniform"
    Just v  -> valueToUniform v uniformType uniformLoc


renderPostProcessingFilter :: FilterVars -> PostFilter -> PostProcessing ()
renderPostProcessingFilter postVars (PostFilter _ _ _ shader quadVBO) = do
  liftIO (currentProgram $= (Just $ ppProgram shader))
  mapM_ (setUniform postVars) (ppUniforms shader)
  liftIO $ drawVBO quadVBO

loadFilterDirectories
  :: [FilePath] -> IO (M.Map String PostProcessingShader, FilterVars)
loadFilterDirectories folders = do
  loadedFolders <- mapM loadShaderFolder folders
  let (folderLoadingErrs, parsedShaders) =
        partitionEithers $ concat $ fmap fst loadedFolders
  mapM_ logError folderLoadingErrs
  loadedFilters <- mapM loadPostProcessingShader parsedShaders
  --let (shaderLoadingErrs, ppShaders) = partitionEithers loadedFilters
  --mapM_ logError shaderLoadingErrs
  let varDefaults = concat $ fmap snd loadedFolders
  logInfo
    $  "Loaded "
    ++ show (length varDefaults)
    ++ " post processing defaults"
  logInfo
    $  "Loaded "
    ++ show (length loadedFilters)
    ++ " post processing filter files"
  return
    $ ( M.fromList $ (\ps -> (ppName ps, ps)) <$> loadedFilters
      , SM.create varDefaults
      )


