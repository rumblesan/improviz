module Gfx.EngineState where

import qualified Data.Map.Strict           as M
import           Data.Vec                  (Mat44, identity, multmm)
import           Graphics.Rendering.OpenGL (GLfloat)
import           Lens.Simple               ((^.))

import           Gfx.Ast                   (Block)
import           Gfx.GeometryBuffers       (GeometryBuffers, createAllBuffers)
import           Gfx.Matrices              (projectionMat, vec3, viewMat)
import           Gfx.PostProcessing        (AnimationStyle, PostProcessing)
import           Gfx.Shaders
import           Gfx.TextRendering         (TextRenderer)
import           Gfx.Textures              (TextureLibrary)
import           Gfx.Types                 (Colour (..))

import           Configuration             (ImprovizConfig)
import qualified Configuration             as C
import qualified Configuration.Screen      as CS

import           Util                      ((/.))

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
  , scopeStack         :: [SavableState]
  } deriving (Show)

data SavableState = SavableState
  { savedMatrixStack  :: [Mat44 GLfloat]
  , savedFillStyles   :: [GFXFillStyling]
  , savedStrokeStyles :: [GFXStrokeStyling]
  } deriving (Show)

data EngineInfo = EngineInfo
  { textureFrames :: M.Map String Int
  } deriving (Show)

createGfxEngineState ::
     ImprovizConfig
  -> Int
  -> Int
  -> PostProcessing
  -> TextRenderer
  -> TextureLibrary
  -> IO EngineState
createGfxEngineState config width height pprocess trender textLib =
  let ratio = width /. height
      front = config ^. C.screen . CS.front
      back = config ^. C.screen . CS.back
      projection = projectionMat front back (pi / 4) ratio
      view = viewMat (vec3 0 0 10) (vec3 0 0 0) (vec3 0 1 0)
   in do gbos <- createAllBuffers
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
             , scopeStack = []
             }

resizeGfxEngine ::
     ImprovizConfig
  -> Int
  -> Int
  -> PostProcessing
  -> TextRenderer
  -> EngineState
  -> EngineState
resizeGfxEngine config newWidth newHeight newPP newTR es =
  let front = config ^. C.screen . CS.front
      back = config ^. C.screen . CS.back
      newRatio = newWidth /. newHeight
      newProj = projectionMat front back (pi / 4) newRatio
   in es {projectionMatrix = newProj, postFX = newPP, textRenderer = newTR}

pushFillStyle :: GFXFillStyling -> EngineState -> EngineState
pushFillStyle s es = es {fillStyles = s : fillStyles es}

currentFillStyle :: EngineState -> GFXFillStyling
currentFillStyle = head . fillStyles

pushStrokeStyle :: GFXStrokeStyling -> EngineState -> EngineState
pushStrokeStyle c es = es {strokeStyles = c : strokeStyles es}

currentStrokeStyle :: EngineState -> GFXStrokeStyling
currentStrokeStyle = head . strokeStyles

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
