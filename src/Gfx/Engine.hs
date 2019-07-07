{-# LANGUAGE TemplateHaskell   #-}

module Gfx.Engine where

import           Control.Monad.State.Strict
import           Data.Vec                       ( Mat44
                                                , identity
                                                , multmm
                                                )
import           Graphics.Rendering.OpenGL      ( GLfloat )
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , set
                                                , (^.)
                                                )

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

data SavableState = SavableState
  { _savedMatrixStack  :: [Mat44 GLfloat]
  , _savedFillStyles   :: [GFXFillStyling]
  , _savedStrokeStyles :: [GFXStrokeStyling]
  } deriving (Show)

makeLenses ''SavableState

data GfxEngine = GfxEngine
  { _fillStyles         :: [GFXFillStyling]
  , _strokeStyles       :: [GFXStrokeStyling]
  , _geometryBuffers    :: GeometryBuffers
  , _textureLibrary     :: TextureLibrary
  , _colourShaders      :: Shaders
  , _strokeShaders      :: Shaders
  , _textureShaders     :: Shaders
  , _viewMatrix         :: Mat44 GLfloat
  , _projectionMatrix   :: Mat44 GLfloat
  , _postFX             :: PostProcessing
  , _textRenderer       :: TextRenderer
  , _matrixStack        :: [Mat44 GLfloat]
  , _scopeStack         :: [SavableState]
  , _animationStyle     :: AnimationStyle
  , _backgroundColor    :: Colour
  } deriving (Show)

makeLenses ''GfxEngine

type GraphicsEngine v = StateT GfxEngine IO v

createGfxEngine
  :: ImprovizConfig
  -> Int
  -> Int
  -> PostProcessing
  -> TextRenderer
  -> TextureLibrary
  -> IO GfxEngine
createGfxEngine config width height pprocess trender textLib =
  let ratio      = width /. height
      front      = config ^. C.screen . CS.front
      back       = config ^. C.screen . CS.back
      projection = projectionMat front back (pi / 4) ratio
      view       = viewMat (vec3 0 0 10) (vec3 0 0 0) (vec3 0 1 0)
  in  do
        gbos <- createAllBuffers
        cshd <- createColourShaders
        tshd <- createTextureShaders
        sshd <- createStrokeShaders
        return GfxEngine { _fillStyles       = [GFXFillColour $ Colour 1 1 1 1]
                         , _strokeStyles = [GFXStrokeColour $ Colour 0 0 0 1]
                         , _geometryBuffers  = gbos
                         , _textureLibrary   = textLib
                         , _colourShaders    = cshd
                         , _strokeShaders    = sshd
                         , _textureShaders   = tshd
                         , _viewMatrix       = view
                         , _projectionMatrix = projection
                         , _postFX           = pprocess
                         , _textRenderer     = trender
                         , _matrixStack      = [identity]
                         , _scopeStack       = []
                         , _animationStyle   = NormalStyle
                         , _backgroundColor  = Colour 1 1 1 1
                         }

resizeGfxEngine
  :: ImprovizConfig
  -> Int
  -> Int
  -> PostProcessing
  -> TextRenderer
  -> GfxEngine
  -> GfxEngine
resizeGfxEngine config newWidth newHeight newPP newTR =
  let front    = config ^. C.screen . CS.front
      back     = config ^. C.screen . CS.back
      newRatio = newWidth /. newHeight
      newProj  = projectionMat front back (pi / 4) newRatio
  in  set projectionMatrix newProj . set postFX newPP . set textRenderer newTR

resetGfxEngine :: GfxEngine -> GfxEngine
resetGfxEngine ge = ge { _fillStyles   = [GFXFillColour $ Colour 1 1 1 1]
                       , _strokeStyles = [GFXStrokeColour $ Colour 0 0 0 1]
                       , _matrixStack  = [identity]
                       , _scopeStack   = []
                       }

pushFillStyle :: GFXFillStyling -> GfxEngine -> GfxEngine
pushFillStyle s = over fillStyles (s :)

pushStrokeStyle :: GFXStrokeStyling -> GfxEngine -> GfxEngine
pushStrokeStyle c = over strokeStyles (c :)

pushMatrix :: Mat44 Float -> GfxEngine -> GfxEngine
pushMatrix mat = over matrixStack (\stack -> multmm (head stack) mat : stack)

popMatrix :: GfxEngine -> GfxEngine
popMatrix = over matrixStack tail

multMatrix :: Mat44 Float -> GfxEngine -> GfxEngine
multMatrix mat =
  over matrixStack (\stack -> multmm (head stack) mat : tail stack)
