module Gfx
  ( renderGfx
  , EngineState(..)
  , Scene(..)
  , createGfxEngineState
  , resizeGfxEngine
  ) where

import           Control.Monad.State.Strict (evalStateT)

import           Graphics.Rendering.OpenGL

import           Gfx.EngineState            (EngineState (..), Scene (..),
                                             createGfxEngineState,
                                             resizeGfxEngine)
import           Gfx.Interpreter            (interpretGfx)
import           Gfx.OpenGL                 (colToGLCol)
import           Gfx.PostProcessing         (AnimationStyle (..),
                                             PostProcessing,
                                             renderPostProcessing,
                                             usePostProcessing)

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
