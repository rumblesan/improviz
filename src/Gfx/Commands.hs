module Gfx.Commands
  ( drawShape
  , rotate
  , scale
  , move
  , textureFill
  , colourFill
  , noFill
  , colourStroke
  , noStroke
  , setMaterial
  , setMaterialVariable
  , setBackground
  , setAnimationStyle
  , setFilterVar
  , setDepthChecking
  , pushScope
  , popScope
  , renderCode
  , renderCodeToBuffer
  ) where

import           Control.Monad.State.Strict     ( modify' )
import           Control.Monad.Trans            ( liftIO )
import qualified Data.Map.Strict               as M
import           Foreign.Marshal.Utils          ( fromBool
                                                , with
                                                )
import           Foreign.Ptr                    ( castPtr )
import           Lens.Simple                    ( assign
                                                , use
                                                , view
                                                )

import           Linear.Matrix                  ( (!*!)
                                                , M44
                                                )

import qualified Graphics.GL                   as GLRaw
import qualified Graphics.Rendering.OpenGL     as GL
import           Graphics.Rendering.OpenGL      ( ($=)
                                                , GLfloat
                                                , TextureTarget2D(Texture2D)
                                                , TextureUnit(..)
                                                , UniformLocation(..)
                                                , currentProgram
                                                )
import           Language.Ast                   ( Value(..) )

import           Gfx.Engine
import           Gfx.Geometries                 ( GeometryBuffers(..) )
import qualified Gfx.Materials                 as GM
import           Gfx.Matrices                   ( rotMat
                                                , scaleMat
                                                , translateMat
                                                )
import           Gfx.OpenGL                     ( colToGLCol
                                                , printErrors
                                                , valueToUniform
                                                )
import qualified Gfx.PostProcessing            as PP
import           Gfx.PostProcessing             ( AnimationStyle(..) )
import           Gfx.TextRendering              ( renderText
                                                , renderTextToBuffer
                                                )
import           Gfx.Types                      ( Colour(..) )
import qualified Gfx.VAO                       as VAO
import           Logging                        ( logError )
import qualified Util.Setting                  as S
import qualified Util.SettingMap               as SM


getFullMatrix :: GraphicsEngine (M44 GLfloat)
getFullMatrix = do
  mMat <- head <$> use matrixStack
  pMat <- use projectionMatrix
  vMat <- use viewMatrix
  return $ (pMat !*! vMat) !*! mMat

setUniform :: (String, GL.VariableType, UniformLocation) -> GraphicsEngine ()
setUniform ("MVPmatrix", _, UniformLocation uniformLoc) = do
  mvpMat <- getFullMatrix
  liftIO
    $ with mvpMat
    $ GLRaw.glUniformMatrix4fv uniformLoc 1 (fromBool True)
    . castPtr
setUniform ("Mmatrix", _, UniformLocation uniformLoc) = do
  modelMatrix <- head <$> use matrixStack
  liftIO
    $ with modelMatrix
    $ GLRaw.glUniformMatrix4fv uniformLoc 1 (fromBool True)
    . castPtr
setUniform ("Vmatrix", _, UniformLocation uniformLoc) = do
  viewMat <- use viewMatrix
  liftIO
    $ with viewMat
    $ GLRaw.glUniformMatrix4fv uniformLoc 1 (fromBool True)
    . castPtr
setUniform ("Pmatrix", _, UniformLocation uniformLoc) = do
  projMat <- use projectionMatrix
  liftIO
    $ with projMat
    $ GLRaw.glUniformMatrix4fv uniformLoc 1 (fromBool True)
    . castPtr
setUniform ("FillColour", _, uniformLoc) = do
  gfxFillStyle <- use (fillStyle . S.value)
  liftIO $ case gfxFillStyle of
    (GFXFillColour fillColour) ->
      GL.uniform uniformLoc $= colToGLCol fillColour
    GFXNoFill -> GL.uniform uniformLoc $= colToGLCol (Colour 0 0 0 (-1))
setUniform ("StrokeColour", _, uniformLoc) = do
  gfxStrokeStyle <- use (strokeStyle . S.value)
  liftIO $ case gfxStrokeStyle of
    (GFXStrokeColour strokeColour) ->
      GL.uniform uniformLoc $= colToGLCol strokeColour
    GFXNoStroke -> GL.uniform uniformLoc $= colToGLCol (Colour 0 0 0 (-1))
setUniform ("Texture", _, uniformLoc) = do
  (GFXTextureStyling textName textFrame) <- use (textureStyle . S.value)
  textureLib                             <- use textureLibrary
  case M.lookup textName textureLib >>= M.lookup textFrame of
    Nothing      -> return ()
    Just texture -> liftIO $ do
      GL.activeTexture $= TextureUnit 0
      GL.textureBinding Texture2D $= Just texture
setUniform (name, uniformType, uniformLoc) = do
  matVar <- use (materialVars . SM.value name)
  liftIO $ case matVar of
    Nothing -> logError $ name ++ " is not a known uniform"
    Just v  -> valueToUniform v uniformType uniformLoc



drawTriangles :: GeometryBuffers -> GraphicsEngine ()
drawTriangles geoData = do
  matName <- use (material . S.value)
  matLib  <- use materialLibrary
  case M.lookup matName matLib of
    Just mat -> do
      liftIO $ VAO.bind (vao geoData)
      liftIO (currentProgram $= Just (GM.program mat))
      mapM_ setUniform (GM.uniforms mat)
      liftIO (GL.polygonMode $= (GL.Fill, GL.Fill))
      liftIO (GL.cullFace $= Just GL.Front)
      liftIO $ VAO.draw $ vao geoData
      liftIO (GL.cullFace $= Just GL.Back)
      liftIO $ VAO.draw $ vao geoData
    _ -> return ()
  liftIO printErrors

drawShape :: String -> Float -> Float -> Float -> GraphicsEngine ()
drawShape name x y z = do
  gbos <- use geometryBuffers
  case M.lookup name gbos of
    Nothing      -> liftIO $ print $ "Could not find shape: " ++ name
    Just geoData -> do
      modify' (pushMatrix $ scaleMat x y z)
      drawTriangles geoData
      modify' popMatrix

rotate :: Float -> Float -> Float -> GraphicsEngine ()
rotate x y z = modify' (multMatrix $ rotMat x y z)

scale :: Float -> Float -> Float -> GraphicsEngine ()
scale x y z = modify' (multMatrix $ scaleMat x y z)

move :: Float -> Float -> Float -> GraphicsEngine ()
move x y z = modify' (multMatrix $ translateMat x y z)

setMaterial :: String -> GraphicsEngine ()
setMaterial = assign (material . S.value)

setMaterialVariable :: String -> Value -> GraphicsEngine ()
setMaterialVariable name value =
  assign (materialVars . SM.value name) (Just value)

setBackground :: Float -> Float -> Float -> GraphicsEngine ()
setBackground r g b = assign (backgroundColor . S.value) (Colour r g b 1)

setAnimationStyle :: AnimationStyle -> GraphicsEngine ()
setAnimationStyle = assign (animationStyle . S.value)

setFilterVar :: String -> Value -> GraphicsEngine ()
setFilterVar name value =
  assign (postFX . PP.filterVars . SM.value name) (Just value)

setDepthChecking :: Bool -> GraphicsEngine ()
setDepthChecking = assign (depthChecking . S.value)

textureFill :: String -> Float -> GraphicsEngine ()
textureFill name frame =
  assign (textureStyle . S.value) $ GFXTextureStyling name (floor frame)

colourFill :: Float -> Float -> Float -> Float -> GraphicsEngine ()
colourFill r g b a =
  assign (fillStyle . S.value) $ GFXFillColour $ Colour r g b a

noFill :: GraphicsEngine ()
noFill = assign (fillStyle . S.value) GFXNoFill

colourStroke :: Float -> Float -> Float -> Float -> GraphicsEngine ()
colourStroke r g b a =
  assign (strokeStyle . S.value) $ GFXStrokeColour $ Colour r g b a

noStroke :: GraphicsEngine ()
noStroke = assign (strokeStyle . S.value) GFXNoStroke

pushScope :: GraphicsEngine ()
pushScope = do
  mStack  <- use matrixStack
  fStyles <- use (fillStyle . S.value)
  sStyles <- use (strokeStyle . S.value)
  texts   <- use (textureStyle . S.value)
  mat     <- use (material . S.value)
  mVars   <- use (materialVars . SM.snapshot)
  stack   <- use scopeStack
  let savable = SavableState mStack fStyles sStyles texts mat mVars
  assign scopeStack (savable : stack)

popScope :: GraphicsEngine ()
popScope = do
  stack <- use scopeStack
  let prev = head stack
  assign scopeStack                   (tail stack)
  assign (fillStyle . S.value)        (view savedFillStyles prev)
  assign (strokeStyle . S.value)      (view savedStrokeStyles prev)
  assign (textureStyle . S.value)     (view savedTextureStyles prev)
  assign matrixStack                  (view savedMatrixStack prev)
  assign (material . S.value)         (view savedMaterials prev)
  assign (materialVars . SM.snapshot) (view savedMaterialVars prev)

renderCode :: String -> GraphicsEngine ()
renderCode text = do
  tr <- use textRenderer
  liftIO $ renderText 0 0 tr text

renderCodeToBuffer :: String -> GraphicsEngine ()
renderCodeToBuffer text = do
  tr <- use textRenderer
  liftIO $ renderTextToBuffer tr
