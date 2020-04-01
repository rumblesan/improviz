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

import           Configuration                  ( ImprovizConfig )

createGfx
  :: ImprovizConfig
  -> TextureLibrary
  -> Int
  -> Int
  -> Int
  -> Int
  -> IO GfxEngine
createGfx config textureLib width height fbWidth fbHeight = do
  post    <- createPostProcessing fbWidth fbHeight
  trender <- createTextRenderer config fbWidth fbHeight
  let tLibWithCode = addCodeTextureToLib trender textureLib
  createGfxEngine config width height post trender tLibWithCode

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
    animStyle  = gs ^. animationStyle
    bgColor    = gs ^. backgroundColor
    depthCheck = gs ^. depthChecking
  in
    do
      usePostProcessing post
      depthFunc $= if depthCheck then Just Less else Nothing
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
      result <- program
      renderPostProcessing post animStyle
      return result
