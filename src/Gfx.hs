module Gfx
  ( GfxEngine
  , renderGfx
  , createGfx
  , resizeGfx
  )
where

import           Lens.Simple                    ( (^.) )

import           Graphics.Rendering.OpenGL

import           Gfx.Engine                     ( GfxEngine
                                                , createGfxEngine
                                                , resizeGfxEngine
                                                , backgroundColor
                                                , depthChecking
                                                , postFX
                                                , animationStyle
                                                , textRenderer
                                                )
import           Gfx.OpenGL                     ( colToGLCol )
import           Gfx.PostProcessing             ( AnimationStyle(..)
                                                , createPostProcessing
                                                , createPostProcessing
                                                , deletePostProcessing
                                                , renderPostProcessing
                                                , usePostProcessing
                                                )
import           Gfx.TextRendering              ( addCodeTextureToLib
                                                , createTextRenderer
                                                , resizeTextRendererScreen
                                                )
import           Gfx.Textures                   ( TextureLibrary )
import qualified Gfx.Materials                 as GM
import qualified Gfx.Setting                   as GS

import           Configuration                  ( ImprovizConfig )
import qualified Configuration                 as C

createGfx
  :: ImprovizConfig
  -> TextureLibrary
  -> Int
  -> Int
  -> Int
  -> Int
  -> IO GfxEngine
createGfx config textureLib width height fbWidth fbHeight = do
  post <- createPostProcessing fbWidth fbHeight
  let scaling = fromIntegral width / fromIntegral fbWidth
  trender     <- createTextRenderer config fbWidth fbHeight scaling
  materialCfg <- GM.createMaterialsConfig (config ^. C.materialDirectories)
  let tLibWithCode = addCodeTextureToLib trender textureLib
  createGfxEngine config width height post trender tLibWithCode materialCfg

resizeGfx
  :: GfxEngine -> ImprovizConfig -> Int -> Int -> Int -> Int -> IO GfxEngine
resizeGfx engineState config newWidth newHeight fbWidth fbHeight = do
  deletePostProcessing $ engineState ^. postFX
  newPost    <- createPostProcessing fbWidth fbHeight
  newTrender <- resizeTextRendererScreen config
                                         fbWidth
                                         fbHeight
                                         (engineState ^. textRenderer)
  return
    $ resizeGfxEngine config newWidth newHeight newPost newTrender engineState

renderGfx :: IO result -> GfxEngine -> IO result
renderGfx program gs =
  let
    post       = gs ^. postFX
    animStyle  = gs ^. animationStyle . GS.value
    bgColor    = gs ^. backgroundColor . GS.value
    depthCheck = gs ^. depthChecking . GS.value
  in
    do
      usePostProcessing post
      depthFunc $= if depthCheck then Just Less else Nothing
      blend $= Enabled
      blendEquationSeparate $= (FuncAdd, FuncAdd)
      frontFace $= CW
      case animStyle of
        NormalStyle ->
          blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, One))
        MotionBlur ->
          blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
        PaintOver ->
          blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
      clearColor $= colToGLCol bgColor
      clear [ColorBuffer, DepthBuffer]
      result <- program
      renderPostProcessing post animStyle
      return result
