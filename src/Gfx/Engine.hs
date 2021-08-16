{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes #-}

module Gfx.Engine where

import           Control.Monad.State.Strict
import qualified Data.Map                      as M
import           Graphics.Rendering.OpenGL      ( GLfloat )
import           Lens.Simple                    ( (^.)
                                                , makeLenses
                                                , over
                                                , set
                                                )
import           Linear.Matrix                  ( (!*!)
                                                , M44
                                                , identity
                                                )
import           Linear.V3                      ( V3(..) )

import           Gfx.Geometries                 ( Geometries
                                                , createAllGeometries
                                                )
import           Gfx.Materials                  ( MaterialLibrary
                                                , MaterialsConfig(..)
                                                )
import           Gfx.Matrices                   ( projectionMat
                                                , viewMat
                                                )
import           Gfx.PostProcessing             ( AnimationStyle(NormalStyle)
                                                , PostProcessingConfig
                                                , filterVars
                                                )
import           Gfx.TextRendering              ( TextRenderer )
import           Gfx.Textures                   ( TextureLibrary )
import           Gfx.Types                      ( Colour(..) )
import           Language.Ast                   ( Value )
import qualified Util.Setting                  as S
import qualified Util.SettingMap               as SM

import           Configuration                  ( ImprovizConfig )
import qualified Configuration                 as C
import qualified Configuration.Screen          as CS

import           Util                           ( (/.) )

data GFXFillStyling
  = GFXFillColour Colour
  | GFXNoFill
  deriving (Eq, Show)

data GFXTextureStyling = GFXTextureStyling String Int
  deriving (Eq, Show)

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
  , _savedMaterialVars  :: M.Map String Value
  }
  deriving Show

makeLenses ''SavableState

data GfxEngine = GfxEngine
  { _fillStyle        :: S.Setting GFXFillStyling
  , _strokeStyle      :: S.Setting GFXStrokeStyling
  , _textureStyle     :: S.Setting GFXTextureStyling
  , _material         :: S.Setting String
  , _geometryBuffers  :: Geometries
  , _textureLibrary   :: TextureLibrary
  , _materialLibrary  :: MaterialLibrary
  , _materialVars     :: SM.SettingMap String Value
  , _viewMatrix       :: M44 GLfloat
  , _projectionMatrix :: M44 GLfloat
  , _postFX           :: PostProcessingConfig
  , _postFXVars       :: SM.SettingMap String Value
  , _textRenderer     :: TextRenderer
  , _matrixStack      :: [M44 GLfloat]
  , _scopeStack       :: [SavableState]
  , _animationStyle   :: S.Setting AnimationStyle
  , _backgroundColor  :: S.Setting Colour
  , _depthChecking    :: S.Setting Bool
  }
  deriving Show

makeLenses '' GfxEngine

type GraphicsEngine v = StateT GfxEngine IO v

createGfxEngine
  :: ImprovizConfig
  -> Int
  -> Int
  -> PostProcessingConfig
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
          { _fillStyle        = S.create $ GFXFillColour $ Colour 1 1 1 1
          , _strokeStyle      = S.create $ GFXStrokeColour $ Colour 0 0 0 1
          , _textureStyle     = S.create $ GFXTextureStyling "" 0
          , _material         = S.create "basic"
          , _geometryBuffers  = gbos
          , _textureLibrary   = textLib
          , _materialLibrary  = materialsLibrary matCfg
          , _materialVars     = SM.create $ varDefaults matCfg
          , _viewMatrix       = view
          , _projectionMatrix = projection
          , _postFX           = pprocess
          , _postFXVars       = pprocess ^. filterVars
          , _textRenderer     = trender
          , _matrixStack      = [identity]
          , _scopeStack       = []
          , _animationStyle   = S.create NormalStyle
          , _backgroundColor  = S.create (Colour 1 1 1 1)
          , _depthChecking    = S.create True
          }

resizeGfxEngine
  :: ImprovizConfig
  -> Int
  -> Int
  -> PostProcessingConfig
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
  { _fillStyle       = S.reset (_fillStyle ge)
  , _strokeStyle     = S.reset (_strokeStyle ge)
  , _material        = S.reset (_material ge)
  , _materialVars    = SM.reset (_materialVars ge)
  , _postFXVars      = SM.reset (_postFXVars ge)
  , _matrixStack     = [identity]
  , _scopeStack      = []
  , _animationStyle  = S.resetIfUnused (_animationStyle ge)
  , _backgroundColor = S.resetIfUnused (_backgroundColor ge)
  , _depthChecking   = S.resetIfUnused (_depthChecking ge)
  }

pushMatrix :: M44 Float -> GfxEngine -> GfxEngine
pushMatrix mat = over matrixStack (\stack -> (head stack !*! mat) : stack)

popMatrix :: GfxEngine -> GfxEngine
popMatrix = over matrixStack tail

multMatrix :: M44 Float -> GfxEngine -> GfxEngine
multMatrix mat = over matrixStack (\stack -> (head stack !*! mat) : tail stack)
