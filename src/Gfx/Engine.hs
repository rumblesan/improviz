{-# LANGUAGE TemplateHaskell   #-}

module Gfx.Engine where

import qualified Data.Map                      as M
import           Control.Monad.State.Strict
import           Linear.V3                      ( V3(..) )
import           Linear.Matrix                  ( M44
                                                , identity
                                                , (!*!)
                                                )
import           Graphics.Rendering.OpenGL      ( GLfloat )
import           Lens.Simple                    ( Lens
                                                , makeLenses
                                                , makeLensesFor
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
import           Gfx.Materials                  ( MaterialLibrary )
import           Gfx.Types                      ( Colour(..) )
import qualified Gfx.Setting                   as GS
import qualified Gfx.SettingStack              as GSS

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
  , _savedFillStyles    :: [GFXFillStyling]
  , _savedStrokeStyles  :: [GFXStrokeStyling]
  , _savedStrokeSize    :: [Float]
  , _savedTextureStyles :: [GFXTextureStyling]
  , _savedMaterials     :: [String]
  } deriving (Show)

makeLenses ''SavableState

data GfxEngine = GfxEngine
  { _fillStyle          :: GSS.SettingStack GFXFillStyling
  , _strokeStyle        :: GSS.SettingStack GFXStrokeStyling
  , _strokeSize         :: GSS.SettingStack Float
  , _textureStyle       :: GSS.SettingStack GFXTextureStyling
  , _material           :: GSS.SettingStack String
  , _geometryBuffers    :: Geometries
  , _textureLibrary     :: TextureLibrary
  , _materialLibrary    :: MaterialLibrary
  , _materialVars       :: M.Map String Float
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

makeLensesFor [ ("_fillStyle", "fillStyleSetting")
              , ("_strokeStyle", "strokeStyleSetting")
              , ("_strokeSize", "strokeSizeSetting")
              , ("_textureStyle", "textureStyleSetting")
              , ("_material", "materialSetting")
              , ("_geometryBuffers", "geometryBuffers")
              , ("_textureLibrary", "textureLibrary")
              , ("_materialLibrary", "materialLibrary")
              , ("_materialVars", "materialVars")
              , ("_viewMatrix", "viewMatrix")
              , ("_projectionMatrix", "projectionMatrix")
              , ("_postFX", "postFX")
              , ("_textRenderer", "textRenderer")
              , ("_matrixStack", "matrixStack")
              , ("_scopeStack", "scopeStack")
              , ("_animationStyle", "animationStyleSetting")
              , ("_backgroundColor", "backgroundColorSetting")
              , ("_depthChecking", "depthCheckingSetting")
              ] ''GfxEngine

material :: Lens GfxEngine GfxEngine String String
material = materialSetting . GSS.value

materialSnapshot :: Lens GfxEngine GfxEngine [String] [String]
materialSnapshot = materialSetting . GSS.snapshot

fillStyle :: Lens GfxEngine GfxEngine GFXFillStyling GFXFillStyling
fillStyle = fillStyleSetting . GSS.value

fillStyleSnapshot :: Lens GfxEngine GfxEngine [GFXFillStyling] [GFXFillStyling]
fillStyleSnapshot = fillStyleSetting . GSS.snapshot

strokeStyle :: Lens GfxEngine GfxEngine GFXStrokeStyling GFXStrokeStyling
strokeStyle = strokeStyleSetting . GSS.value

strokeStyleSnapshot
  :: Lens GfxEngine GfxEngine [GFXStrokeStyling] [GFXStrokeStyling]
strokeStyleSnapshot = strokeStyleSetting . GSS.snapshot

strokeSize :: Lens GfxEngine GfxEngine Float Float
strokeSize = strokeSizeSetting . GSS.value

strokeSizeSnapshot :: Lens GfxEngine GfxEngine [Float] [Float]
strokeSizeSnapshot = strokeSizeSetting . GSS.snapshot

textureStyle :: Lens GfxEngine GfxEngine GFXTextureStyling GFXTextureStyling
textureStyle = textureStyleSetting . GSS.value

textureStyleSnapshot
  :: Lens GfxEngine GfxEngine [GFXTextureStyling] [GFXTextureStyling]
textureStyleSnapshot = textureStyleSetting . GSS.snapshot


animationStyle :: Lens GfxEngine GfxEngine AnimationStyle AnimationStyle
animationStyle = animationStyleSetting . GS.value

backgroundColor :: Lens GfxEngine GfxEngine Colour Colour
backgroundColor = backgroundColorSetting . GS.value

depthChecking :: Lens GfxEngine GfxEngine Bool Bool
depthChecking = depthCheckingSetting . GS.value

type GraphicsEngine v = StateT GfxEngine IO v

createGfxEngine
  :: ImprovizConfig
  -> Int
  -> Int
  -> PostProcessing
  -> TextRenderer
  -> TextureLibrary
  -> MaterialLibrary
  -> IO GfxEngine
createGfxEngine config width height pprocess trender textLib matLib =
  let ratio      = width /. height
      front      = config ^. C.screen . CS.front
      back       = config ^. C.screen . CS.back
      projection = projectionMat front back (pi / 4) ratio
      view       = viewMat (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)
  in  do
        gbos <- createAllGeometries (config ^. C.geometryDirectories)
        return GfxEngine
          { _fillStyle        = GSS.create $ GFXFillColour $ Colour 1 1 1 1
          , _strokeStyle      = GSS.create $ GFXStrokeColour $ Colour 0 0 0 1
          , _strokeSize       = GSS.create 0.02
          , _textureStyle     = GSS.create $ GFXTextureStyling "" 0
          , _material         = GSS.create "basic"
          , _geometryBuffers  = gbos
          , _textureLibrary   = textLib
          , _materialLibrary  = matLib
          , _materialVars     = M.empty
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
resetGfxEngine ge = ge { _fillStyle       = GSS.reset (_fillStyle ge)
                       , _strokeStyle     = GSS.reset (_strokeStyle ge)
                       , _strokeSize      = GSS.reset (_strokeSize ge)
                       , _material        = GSS.reset (_material ge)
                       , _materialVars    = M.empty
                       , _matrixStack     = [identity]
                       , _scopeStack      = []
                       , _animationStyle  = GS.reset (_animationStyle ge)
                       , _backgroundColor = GS.reset (_backgroundColor ge)
                       , _depthChecking   = GS.reset (_depthChecking ge)
                       }

pushMatrix :: M44 Float -> GfxEngine -> GfxEngine
pushMatrix mat = over matrixStack (\stack -> (head stack !*! mat) : stack)

popMatrix :: GfxEngine -> GfxEngine
popMatrix = over matrixStack tail

multMatrix :: M44 Float -> GfxEngine -> GfxEngine
multMatrix mat = over matrixStack (\stack -> (head stack !*! mat) : tail stack)
