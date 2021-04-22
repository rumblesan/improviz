{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes #-}

module Gfx.Engine where

import qualified Data.Map                      as M
import           Control.Monad.State.Strict
import           Linear.V3                      ( V3(..) )
import           Linear.Matrix                  ( M44
                                                , identity
                                                , (!*!)
                                                )
import           Graphics.Rendering.OpenGL      ( GLfloat )
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , set
                                                , (^.)
                                                )

import           Gfx.Geometries                 ( Geometries
                                                , createAllGeometries
                                                )
import           Gfx.Matrices                   ( projectionMat
                                                , viewMat
                                                )
import           Gfx.PostProcessing             ( AnimationStyle(NormalStyle)
                                                , PostProcessing
                                                )
import           Gfx.TextRendering              ( TextRenderer )
import           Gfx.Textures                   ( TextureLibrary )
import           Gfx.Materials                  ( MaterialsConfig(..), MaterialLibrary )
import           Gfx.Types                      ( Colour(..) )
import qualified Gfx.Setting                   as GS
import qualified Gfx.SettingMap                as GSM

import           Configuration                  ( ImprovizConfig )
import qualified Configuration                 as C
import qualified Configuration.Screen          as CS

import           Util                           ( (/.) )

data GFXFillStyling
  = GFXFillColour Colour
  | GFXNoFill
  deriving (Eq, Show)

data GFXTextureStyling = GFXTextureStyling String Int deriving (Eq, Show)

data GFXStrokeStyling
  = GFXStrokeColour Colour
  | GFXNoStroke
  deriving (Eq, Show)

data SavableState = SavableState
  { _savedMatrixStack   :: [M44 GLfloat]
  , _savedFillStyles    :: GFXFillStyling
  , _savedStrokeStyles  :: GFXStrokeStyling
  , _savedTextureStyles :: GFXTextureStyling
  , _savedMaterials     :: String
  , _savedMaterialVars  :: M.Map String Float
  } deriving (Show)

makeLenses ''SavableState

data GfxEngine = GfxEngine
  { _fillStyle          :: GS.Setting GFXFillStyling
  , _strokeStyle        :: GS.Setting GFXStrokeStyling
  , _textureStyle       :: GS.Setting GFXTextureStyling
  , _material           :: GS.Setting String
  , _geometryBuffers    :: Geometries
  , _textureLibrary     :: TextureLibrary
  , _materialLibrary    :: MaterialLibrary
  , _materialVars       :: GSM.SettingMap String Float
  , _viewMatrix         :: M44 GLfloat
  , _projectionMatrix   :: M44 GLfloat
  , _postFX             :: PostProcessing
  , _textRenderer       :: TextRenderer
  , _matrixStack        :: [M44 GLfloat]
  , _scopeStack         :: [SavableState]
  , _animationStyle     :: GS.Setting AnimationStyle
  , _backgroundColor    :: GS.Setting Colour
  , _depthChecking    :: GS.Setting Bool
  } deriving (Show)

makeLenses '' GfxEngine

type GraphicsEngine v = StateT GfxEngine IO v

createGfxEngine
  :: ImprovizConfig
  -> Int
  -> Int
  -> PostProcessing
  -> TextRenderer
  -> TextureLibrary
  -> MaterialsConfig
  -> IO GfxEngine
createGfxEngine config width height pprocess trender textLib matCfg =
  let ratio      = width /. height
      front      = config ^. C.screen . CS.front
      back       = config ^. C.screen . CS.back
      projection = projectionMat front back (pi / 4) ratio
      view       = viewMat (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)
  in  do
        gbos <- createAllGeometries (config ^. C.geometryDirectories)
        return GfxEngine
          { _fillStyle        = GS.create $ GFXFillColour $ Colour 1 1 1 1
          , _strokeStyle      = GS.create $ GFXStrokeColour $ Colour 0 0 0 1
          , _textureStyle     = GS.create $ GFXTextureStyling "" 0
          , _material         = GS.create "basic"
          , _geometryBuffers  = gbos
          , _textureLibrary   = textLib
          , _materialLibrary  = materialsLibrary matCfg
          , _materialVars     = GSM.create $ varDefaults matCfg
          , _viewMatrix       = view
          , _projectionMatrix = projection
          , _postFX           = pprocess
          , _textRenderer     = trender
          , _matrixStack      = [identity]
          , _scopeStack       = []
          , _animationStyle   = GS.create NormalStyle
          , _backgroundColor  = GS.create (Colour 1 1 1 1)
          , _depthChecking    = GS.create True
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
resetGfxEngine ge = ge
  { _fillStyle       = GS.reset (_fillStyle ge)
  , _strokeStyle     = GS.reset (_strokeStyle ge)
  , _material        = GS.reset (_material ge)
  , _materialVars    = GSM.reset (_materialVars ge)
  , _matrixStack     = [identity]
  , _scopeStack      = []
  , _animationStyle  = GS.resetIfUnused (_animationStyle ge)
  , _backgroundColor = GS.resetIfUnused (_backgroundColor ge)
  , _depthChecking   = GS.resetIfUnused (_depthChecking ge)
  }

pushMatrix :: M44 Float -> GfxEngine -> GfxEngine
pushMatrix mat = over matrixStack (\stack -> (head stack !*! mat) : stack)

popMatrix :: GfxEngine -> GfxEngine
popMatrix = over matrixStack tail

multMatrix :: M44 Float -> GfxEngine -> GfxEngine
multMatrix mat = over matrixStack (\stack -> (head stack !*! mat) : tail stack)
