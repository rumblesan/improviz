{-# LANGUAGE TemplateHaskell   #-}

module Gfx.EngineState where

import           Data.Vec                       ( Mat44
                                                , identity
                                                , multmm
                                                )
import           Graphics.Rendering.OpenGL      ( GLfloat )
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , view
                                                , set
                                                , (^.)
                                                )

import           Gfx.Ast                        ( Block )
import           Gfx.GeometryBuffers            ( GeometryBuffers
                                                , createAllBuffers
                                                )
import           Gfx.Matrices                   ( projectionMat
                                                , vec3
                                                , viewMat
                                                )
import           Gfx.PostProcessing             ( AnimationStyle(NormalStyle)
                                                , PostProcessing
                                                )
import           Gfx.Shaders
import           Gfx.TextRendering              ( TextRenderer )
import           Gfx.Textures                   ( TextureLibrary )
import           Gfx.Types                      ( Colour(..) )

import           Configuration                  ( ImprovizConfig )
import qualified Configuration                 as C
import qualified Configuration.Screen          as CS

import           Util                           ( (/.) )

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

newtype Scene = Scene { sceneGfx :: Block }

data SavableState = SavableState
  { _savedMatrixStack  :: [Mat44 GLfloat]
  , _savedFillStyles   :: [GFXFillStyling]
  , _savedStrokeStyles :: [GFXStrokeStyling]
  } deriving (Show)

makeLenses ''SavableState

data EngineState = EngineState
  { _fillStyles         :: [GFXFillStyling]
  , _strokeStyles       :: [GFXStrokeStyling]
  , _drawTransparencies :: Bool
  , _geometryBuffers    :: GeometryBuffers
  , _textureLibrary     :: TextureLibrary
  , _colourShaders      :: Shaders
  , _textureShaders     :: Shaders
  , _viewMatrix         :: Mat44 GLfloat
  , _projectionMatrix   :: Mat44 GLfloat
  , _postFX             :: PostProcessing
  , _textRenderer       :: TextRenderer
  , _matrixStack        :: [Mat44 GLfloat]
  , _scopeStack         :: [SavableState]
  , _animationStyle   :: AnimationStyle
  , _backgroundColor    :: Colour
  } deriving (Show)

makeLenses ''EngineState

createGfxEngineState
  :: ImprovizConfig
  -> Int
  -> Int
  -> PostProcessing
  -> TextRenderer
  -> TextureLibrary
  -> IO EngineState
createGfxEngineState config width height pprocess trender textLib =
  let ratio      = width /. height
      front      = config ^. C.screen . CS.front
      back       = config ^. C.screen . CS.back
      projection = projectionMat front back (pi / 4) ratio
      view       = viewMat (vec3 0 0 10) (vec3 0 0 0) (vec3 0 1 0)
  in  do
        gbos <- createAllBuffers
        cshd <- createColourShaders
        tshd <- createTextureShaders
        return EngineState { _fillStyles = [GFXFillColour $ Colour 1 1 1 1]
                           , _strokeStyles = [GFXStrokeColour $ Colour 0 0 0 1]
                           , _drawTransparencies = False
                           , _geometryBuffers    = gbos
                           , _textureLibrary     = textLib
                           , _colourShaders      = cshd
                           , _textureShaders     = tshd
                           , _viewMatrix         = view
                           , _projectionMatrix   = projection
                           , _postFX             = pprocess
                           , _textRenderer       = trender
                           , _matrixStack        = [identity]
                           , _scopeStack         = []
                           , _animationStyle     = NormalStyle
                           , _backgroundColor    = Colour 1 1 1 1
                           }

resizeGfxEngine
  :: ImprovizConfig
  -> Int
  -> Int
  -> PostProcessing
  -> TextRenderer
  -> EngineState
  -> EngineState
resizeGfxEngine config newWidth newHeight newPP newTR =
  let front    = config ^. C.screen . CS.front
      back     = config ^. C.screen . CS.back
      newRatio = newWidth /. newHeight
      newProj  = projectionMat front back (pi / 4) newRatio
  in  set projectionMatrix newProj . set postFX newPP . set textRenderer newTR

pushFillStyle :: GFXFillStyling -> EngineState -> EngineState
pushFillStyle s = over fillStyles (s :)

currentFillStyle :: EngineState -> GFXFillStyling
currentFillStyle = head . view fillStyles

pushStrokeStyle :: GFXStrokeStyling -> EngineState -> EngineState
pushStrokeStyle c = over strokeStyles (c :)

currentStrokeStyle :: EngineState -> GFXStrokeStyling
currentStrokeStyle = head . view strokeStyles

pushMatrix :: EngineState -> Mat44 Float -> EngineState
pushMatrix es mat =
  let stack = view matrixStack es
      comp  = multmm (head stack) mat
  in  set matrixStack (comp : stack) es

popMatrix :: EngineState -> EngineState
popMatrix = over matrixStack tail

modelMatrix :: EngineState -> Mat44 Float
modelMatrix = head . view matrixStack

multMatrix :: EngineState -> Mat44 Float -> EngineState
multMatrix es mat =
  let stack   = view matrixStack es
      newhead = multmm (head stack) mat
  in  set matrixStack (newhead : tail stack) es
