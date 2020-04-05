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
  , setBackground
  , setAnimationStyle
  , setDepthChecking
  , pushScope
  , popScope
  , renderCode
  , renderCodeToBuffer
  )
where

import           Foreign.Ptr                    ( nullPtr
                                                , castPtr
                                                )
import           Foreign.Marshal.Utils          ( with
                                                , fromBool
                                                )
import           Control.Monad.Trans            ( liftIO )
import           Control.Monad.State.Strict     ( modify' )
import           Lens.Simple                    ( use
                                                , assign
                                                , view
                                                )
import qualified Data.Map.Strict               as M

import           Linear.Matrix                  ( M44
                                                , (!*!)
                                                )

import qualified Graphics.GL                   as GLRaw
import qualified Graphics.Rendering.OpenGL     as GL
import           Graphics.Rendering.OpenGL      ( ($=)
                                                , GLfloat
                                                , TextureUnit(..)
                                                , TextureTarget2D(Texture2D)
                                                , AttribLocation
                                                , UniformLocation(..)
                                                , currentProgram
                                                )

import           Gfx.Engine
import           Gfx.Geometries                 ( GeometryData(..) )
import           Gfx.Matrices                   ( scaleMat
                                                , translateMat
                                                , rotMat
                                                )
import qualified Gfx.Materials                 as GM
import qualified Gfx.VAO                       as VAO
import           Gfx.Types                      ( Colour(..) )
import           Gfx.PostProcessing             ( AnimationStyle(..) )
import           Gfx.TextRendering              ( renderText
                                                , renderTextToBuffer
                                                )
import           Gfx.OpenGL                     ( printErrors
                                                , colToGLCol
                                                )
import           Logging                        ( logError )


getFullMatrix :: GraphicsEngine (M44 GLfloat)
getFullMatrix = do
  mMat <- head <$> use matrixStack
  pMat <- use projectionMatrix
  vMat <- use viewMatrix
  return $ (pMat !*! vMat) !*! mMat

setUniform :: (String, UniformLocation) -> GraphicsEngine ()
setUniform ("MVPMat", UniformLocation uniformLoc) = do
  mvpMat <- getFullMatrix
  liftIO
    $ with mvpMat
    $ GLRaw.glUniformMatrix4fv uniformLoc 1 (fromBool True)
    . castPtr
setUniform ("Color", uniformLoc) = do
  (GFXFillColour fillColour) <- use fillStyle
  liftIO (GL.uniform uniformLoc $= colToGLCol fillColour)
setUniform ("Texture", uniformLoc) = do
  (GFXTextureStyling textName textFrame) <- use textureStyle
  textureLib                             <- use textureLibrary
  case M.lookup textName textureLib >>= M.lookup textFrame of
    Nothing      -> return ()
    Just texture -> liftIO $ do
      GL.activeTexture $= TextureUnit 0
      GL.textureBinding Texture2D $= Just texture
setUniform (name, _) = liftIO $ logError $ name ++ " is not a known uniform"

drawTriangles :: GeometryData -> GraphicsEngine ()
drawTriangles geoData = do
  matName <- use material
  matLib  <- use materialLibrary
  case M.lookup matName matLib of
    Just mat -> do
      liftIO $ VAO.bind (vao geoData)
      liftIO (currentProgram $= Just (GM.program mat))
      mapM_ setUniform (GM.uniforms mat)
      liftIO (GL.polygonMode $= (GL.Fill, GL.Fill))
      liftIO $ GL.drawArrays GL.Triangles 0 (vertCount geoData)
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
setMaterial = assign material

setBackground :: Float -> Float -> Float -> GraphicsEngine ()
setBackground r g b = assign backgroundColor (Colour r g b 1)

setAnimationStyle :: AnimationStyle -> GraphicsEngine ()
setAnimationStyle = assign animationStyle

setDepthChecking :: Bool -> GraphicsEngine ()
setDepthChecking = assign depthChecking

textureFill :: String -> Float -> GraphicsEngine ()
textureFill name frame =
  assign textureStyle $ GFXTextureStyling name (floor frame)

colourFill :: Float -> Float -> Float -> Float -> GraphicsEngine ()
colourFill r g b a = assign fillStyle $ GFXFillColour $ Colour r g b a

noFill :: GraphicsEngine ()
noFill = assign fillStyle $ GFXFillColour $ Colour 0.0 0.0 0.0 0.0

colourStroke :: Float -> Float -> Float -> Float -> GraphicsEngine ()
colourStroke r g b a = assign strokeStyle $ GFXStrokeColour $ Colour r g b a

noStroke :: GraphicsEngine ()
noStroke = assign strokeStyle $ GFXStrokeColour $ Colour 0.0 0.0 0.0 0.0

pushScope :: GraphicsEngine ()
pushScope = do
  mStack  <- use matrixStack
  fStyles <- use fillStyleSnapshot
  sStyles <- use strokeStyleSnapshot
  texts   <- use textureStyleSnapshot
  mat     <- use materialSnapshot
  stack   <- use scopeStack
  let savable = SavableState mStack fStyles sStyles texts mat
  assign scopeStack (savable : stack)

popScope :: GraphicsEngine ()
popScope = do
  stack <- use scopeStack
  let prev = head stack
  assign scopeStack           (tail stack)
  assign fillStyleSnapshot    (view savedFillStyles prev)
  assign strokeStyleSnapshot  (view savedStrokeStyles prev)
  assign textureStyleSnapshot (view savedTextureStyles prev)
  assign matrixStack          (view savedMatrixStack prev)
  assign materialSnapshot     (view savedMaterials prev)

renderCode :: String -> GraphicsEngine ()
renderCode text = do
  tr <- use textRenderer
  liftIO $ renderText 0 0 tr text

renderCodeToBuffer :: String -> GraphicsEngine ()
renderCodeToBuffer text = do
  tr <- use textRenderer
  liftIO $ renderTextToBuffer tr
