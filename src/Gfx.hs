module Gfx
  ( GfxEngine
  , renderGfx
  , createGfx
  , resizeGfx
  ) where

import           Lens.Simple                    ( (^.) )

import           Graphics.Rendering.OpenGL

import           Gfx.Engine                     ( GfxEngine
                                                , animationStyle
                                                , backgroundColor
                                                , blendFunction
                                                , createGfxEngine
                                                , depthChecking
                                                , postFX
                                                , resizeGfxEngine
                                                , textRenderer
                                                )
import qualified Gfx.Materials                 as GM
import           Gfx.OpenGL                     ( colToGLCol )
import           Gfx.PostProcessing             ( createPostProcessing
                                                , deletePostProcessing
                                                , renderPostProcessing
                                                , usePostProcessing
                                                )
import           Gfx.TextRendering              ( addCodeTextureToLib
                                                , createTextRenderer
                                                , resizeTextRendererScreen
                                                )
import           Gfx.Textures                   ( TextureLibrary )
import qualified Util.Setting                  as S

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
  post <- createPostProcessing (config ^. C.filterDirectories) fbWidth fbHeight
  let scaling = fromIntegral width / fromIntegral fbWidth
  trender     <- createTextRenderer config fbWidth fbHeight scaling
  materialCfg <- GM.createMaterialsConfig (config ^. C.materialDirectories)
  let tLibWithCode = addCodeTextureToLib trender textureLib
  createGfxEngine config width height post trender tLibWithCode materialCfg

resizeGfx
  :: GfxEngine -> ImprovizConfig -> Int -> Int -> Int -> Int -> IO GfxEngine
resizeGfx engineState config newWidth newHeight fbWidth fbHeight = do
  deletePostProcessing $ engineState ^. postFX
  newPost <- createPostProcessing (config ^. C.filterDirectories)
                                  fbWidth
                                  fbHeight
  newTrender <- resizeTextRendererScreen config
                                         fbWidth
                                         fbHeight
                                         (engineState ^. textRenderer)
  return
    $ resizeGfxEngine config newWidth newHeight newPost newTrender engineState

renderGfx :: IO result -> GfxEngine -> IO result
renderGfx program gs =
  let post       = gs ^. postFX
      animStyle  = gs ^. animationStyle . S.value
      bgColor    = gs ^. backgroundColor . S.value
      depthCheck = gs ^. depthChecking . S.value
      blendFunc  = gs ^. blendFunction . S.value
  in  do
        usePostProcessing post
        depthFunc $= if depthCheck then Just Less else Nothing
        blend $= Enabled
        blendEquationSeparate $= (FuncAdd, FuncAdd)
        frontFace $= CW
        blendFuncSeparate $= blendFunc
        clearColor $= colToGLCol bgColor
        clear [ColorBuffer, DepthBuffer]
        result <- program
        renderPostProcessing post animStyle
        return result
