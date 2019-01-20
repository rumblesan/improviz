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
import           Gfx.Types                 (Colour (..))

data GFXFillStyling
  = GFXFillColour Colour
  | GFXFillTexture String
                   Int
  | GFXNoFill
  deriving (Eq, Show)

data GFXStrokeStyling
  = GFXStrokeColour Colour
  | GFXNoStroke
  deriving (Eq, Show)

data Scene = Scene
  { sceneBackground       :: Colour
  , sceneGfx              :: Block
  , scenePostProcessingFX :: AnimationStyle
  }

data EngineState = EngineState
  { fillStyles         :: [GFXFillStyling]
  , strokeStyles       :: [GFXStrokeStyling]
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
      { fillStyles = [GFXFillColour $ Colour 1 1 1 1]
      , strokeStyles = [GFXStrokeColour $ Colour 0 0 0 1]
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

pushFillStyle :: GFXFillStyling -> EngineState -> EngineState
pushFillStyle s es = es {fillStyles = s : fillStyles es}

currentFillStyle :: EngineState -> GFXFillStyling
currentFillStyle = head . fillStyles

popFillStyle :: EngineState -> EngineState
popFillStyle es = es {fillStyles = tail $ fillStyles es}

pushStrokeStyle :: GFXStrokeStyling -> EngineState -> EngineState
pushStrokeStyle c es = es {strokeStyles = c : strokeStyles es}

currentStrokeStyle :: EngineState -> GFXStrokeStyling
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
