module Gfx
  ( EngineState(..)
  , Scene(..)
  , renderGfx
  , emptyGfx
  , createGfx
  , resizeGfx
  )
where

import           Control.Monad.State.Strict     ( evalStateT )

import           Lens.Simple                    ( (^.) )

import           Graphics.Rendering.OpenGL

import           Gfx.EngineState                ( EngineState(..)
                                                , Scene(..)
                                                , createGfxEngineState
                                                , resizeGfxEngine
                                                )
import           Gfx.Interpreter                ( interpretGfx )
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
import           Gfx.Textures                   ( createTextureLib )

import           Configuration                  ( ImprovizConfig )
import qualified Configuration                 as C

createGfx :: ImprovizConfig -> Int -> Int -> Int -> Int -> IO EngineState
createGfx config width height fbWidth fbHeight = do
  post         <- createPostProcessing fbWidth fbHeight
  textRenderer <- createTextRenderer config fbWidth fbHeight
  textureLib   <- createTextureLib (config ^. C.textureDirectories)
  let tLibWithCode = addCodeTextureToLib textRenderer textureLib
  createGfxEngineState config width height post textRenderer tLibWithCode

resizeGfx
  :: EngineState -> ImprovizConfig -> Int -> Int -> Int -> Int -> IO EngineState
resizeGfx engineState config newWidth newHeight fbWidth fbHeight = do
  deletePostProcessing $ postFX engineState
  newPost    <- createPostProcessing fbWidth fbHeight
  newTrender <- resizeTextRendererScreen config
                                         fbWidth
                                         fbHeight
                                         (textRenderer engineState)
  return
    $ resizeGfxEngine config newWidth newHeight newPost newTrender engineState

renderGfx :: EngineState -> Scene -> IO ()
renderGfx gs scene =
  let
    post      = postFX gs
    animStyle = postProcessingFX gs
    bgColor   = backgroundColor gs
  in
    do
      usePostProcessing post
      depthFunc $= Just Less
      blend $= Enabled
      blendEquationSeparate $= (FuncAdd, FuncAdd)
      case animStyle of
        NormalStyle ->
          blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, One))
        MotionBlur ->
          blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
        PaintOver ->
          blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
      clearColor $= colToGLCol bgColor
      clear [ColorBuffer, DepthBuffer]
      evalStateT (interpretGfx $ sceneGfx scene) gs
      renderPostProcessing post animStyle
