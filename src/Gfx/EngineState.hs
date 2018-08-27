module Gfx.EngineState where

import qualified Data.Map.Strict           as M
import           Data.Vec                  (Mat44, identity, multmm)
import           Graphics.Rendering.OpenGL (Color4 (..), GLfloat)

import           Gfx.Ast                   (Block)
import           Gfx.GeometryBuffers       (GeometryBuffers, createAllBuffers)
import           Gfx.PostProcessing        (AnimationStyle, PostProcessing)
import           Gfx.Shaders
import           Gfx.TextRendering         (TextRenderer)
import           Gfx.Textures              (TextureLibrary)

data GFXStyling
  = GFXColour (Color4 GLfloat)
  | GFXTexture String
               Int
  deriving (Show)

data Scene = Scene
  { sceneBackground       :: Color4 Float
  , sceneGfx              :: Block
  , scenePostProcessingFX :: AnimationStyle
  }

data EngineState = EngineState
  { fillStyles         :: [GFXStyling]
  , strokeStyles       :: [Color4 GLfloat]
  , drawTransparencies :: Bool
  , geometryBuffers    :: GeometryBuffers
  , textureLibrary     :: TextureLibrary
  , colourShaders      :: Shaders
  , textureShaders     :: Shaders
  , viewMatrix         :: Mat44 GLfloat
  , projectionMatrix   :: Mat44 GLfloat
  , postFX             :: PostProcessing
  , textRenderer       :: TextRenderer
  , matrixStack        :: [Mat44 GLfloat]
  } deriving (Show)

data EngineInfo = EngineInfo
  { textureFrames :: M.Map String Int
  } deriving (Show)

createGfxEngineState ::
     Mat44 GLfloat
  -> Mat44 GLfloat
  -> PostProcessing
  -> TextRenderer
  -> TextureLibrary
  -> IO EngineState
createGfxEngineState projection view pprocess trender textLib = do
  gbos <- createAllBuffers
  cshd <- createColourShaders
  tshd <- createTextureShaders
  return
    EngineState
      { fillStyles = [GFXColour $ Color4 1 1 1 1]
      , strokeStyles = [Color4 0 0 0 1]
      , drawTransparencies = False
      , geometryBuffers = gbos
      , textureLibrary = textLib
      , colourShaders = cshd
      , textureShaders = tshd
      , viewMatrix = view
      , projectionMatrix = projection
      , postFX = pprocess
      , textRenderer = trender
      , matrixStack = [identity]
      }

createEngineInfo :: EngineState -> EngineInfo
createEngineInfo es = EngineInfo $ M.size <$> textureLibrary es

pushFillStyle :: GFXStyling -> EngineState -> EngineState
pushFillStyle s es = es {fillStyles = s : fillStyles es}

currentFillStyle :: EngineState -> GFXStyling
currentFillStyle = head . fillStyles

popFillStyle :: EngineState -> EngineState
popFillStyle es = es {fillStyles = tail $ fillStyles es}

pushStrokeStyle :: Color4 GLfloat -> EngineState -> EngineState
pushStrokeStyle c es = es {strokeStyles = c : strokeStyles es}

currentStrokeStyle :: EngineState -> Color4 GLfloat
currentStrokeStyle = head . strokeStyles

popStrokeStyles :: EngineState -> EngineState
popStrokeStyles es = es {strokeStyles = tail $ strokeStyles es}

pushMatrix :: EngineState -> Mat44 Float -> EngineState
pushMatrix es mat =
  let stack = matrixStack es
      comp = multmm (head stack) mat
   in es {matrixStack = comp : stack}

popMatrix :: EngineState -> EngineState
popMatrix es = es {matrixStack = tail $ matrixStack es}

modelMatrix :: EngineState -> Mat44 Float
modelMatrix = head . matrixStack

multMatrix :: EngineState -> Mat44 Float -> EngineState
multMatrix es mat =
  let stack = matrixStack es
      newhead = multmm (head stack) mat
   in es {matrixStack = newhead : tail stack}

dupeHeadMatrix :: EngineState -> EngineState
dupeHeadMatrix es = es {matrixStack = head (matrixStack es) : matrixStack es}
