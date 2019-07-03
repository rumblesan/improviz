module Gfx
  ( GfxEngine
  , renderGfx
  , createGfx
  , resizeGfx
  , updateGfx
  , renderCode
  , renderCodeToBuffer
  , textureLibrary
  )
where

import           Lens.Simple                    ( (^.) )

import           Graphics.Rendering.OpenGL

import           Gfx.Engine                     ( GfxEngine
                                                , createGfxEngine
                                                , resizeGfxEngine
                                                , updateGfxEngine
                                                , backgroundColor
                                                , postFX
                                                , animationStyle
                                                , textRenderer
                                                , textureLibrary
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
                                                , renderText
                                                , renderTextToBuffer
                                                )
import           Gfx.Textures                   ( createTextureLib )

import           Configuration                  ( ImprovizConfig )
import qualified Configuration                 as C

createGfx :: ImprovizConfig -> Int -> Int -> Int -> Int -> IO GfxEngine
createGfx config width height fbWidth fbHeight = do
  post         <- createPostProcessing fbWidth fbHeight
  textRenderer <- createTextRenderer config fbWidth fbHeight
  textureLib   <- createTextureLib (config ^. C.textureDirectories)
  let tLibWithCode = addCodeTextureToLib textRenderer textureLib
  createGfxEngine config width height post textRenderer tLibWithCode

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
    post      = gs ^. postFX
    animStyle = gs ^. animationStyle
    bgColor   = gs ^. backgroundColor
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
      result <- program
      renderPostProcessing post animStyle
      return result

updateGfx = updateGfxEngine

renderCode :: GfxEngine -> String -> IO ()
renderCode gfxEngine = renderText 0 0 (gfxEngine ^. textRenderer)

renderCodeToBuffer :: GfxEngine -> String -> IO ()
renderCodeToBuffer gfxEngine codeText = do
  renderText 0 0 (gfxEngine ^. textRenderer) codeText
  renderTextToBuffer (gfxEngine ^. textRenderer)
