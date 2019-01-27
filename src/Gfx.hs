module Gfx
  ( EngineState(..)
  , Scene(..)
  , renderGfx
  , emptyGfx
  , createGfxEngine
  , resizeGfx
  ) where

import           Control.Monad.State.Strict (evalStateT)

import           Lens.Simple                ((^.))

import           Graphics.Rendering.OpenGL

import           Gfx.EngineState            (EngineState (..), Scene (..),
                                             createGfxEngineState,
                                             resizeGfxEngine)
import           Gfx.Interpreter            (interpretGfx)
import           Gfx.OpenGL                 (colToGLCol)
import           Gfx.PostProcessing         (AnimationStyle (..),
                                             PostProcessing,
                                             createPostProcessing,
                                             createPostProcessing,
                                             deletePostProcessing,
                                             renderPostProcessing,
                                             usePostProcessing)
import           Gfx.TextRendering          (TextRenderer, addCodeTextureToLib,
                                             createTextRenderer, renderCode,
                                             renderCodebuffer,
                                             resizeTextRendererScreen)
import           Gfx.Textures               (addTexture, createTextureLib)

import           Configuration              (ImprovizConfig)
import qualified Configuration              as C

emptyGfx :: EngineState
emptyGfx = undefined

createGfxEngine :: ImprovizConfig -> Int -> Int -> IO EngineState
createGfxEngine config width height = do
  post <- createPostProcessing width height
  textRenderer <- createTextRenderer config width height
  textureLib <- createTextureLib (config ^. C.textureDirectories)
  let tLibWithCode = addCodeTextureToLib textRenderer textureLib
  createGfxEngineState config width height post textRenderer tLibWithCode

resizeGfx ::
     EngineState -> ImprovizConfig -> Int -> Int -> Int -> Int -> IO EngineState
resizeGfx engineState config newWidth newHeight fbWidth fbHeight = do
  deletePostProcessing $ postFX engineState
  newPost <- createPostProcessing fbWidth fbHeight
  newTrender <-
    resizeTextRendererScreen config fbWidth fbHeight (textRenderer engineState)
  return $
    resizeGfxEngine config newWidth newHeight newPost newTrender engineState

renderGfx :: EngineState -> Scene -> IO ()
renderGfx gs scene =
  let post = postFX gs
      animStyle = scenePostProcessingFX scene
   in do usePostProcessing post
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
         clearColor $= colToGLCol (sceneBackground scene)
         clear [ColorBuffer, DepthBuffer]
         evalStateT (interpretGfx $ sceneGfx scene) gs
         renderPostProcessing post $ scenePostProcessingFX scene
