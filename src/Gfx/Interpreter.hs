module Gfx.Interpreter
  ( GraphicsEngine
  , runGfx
  , drawLine
  , drawRectangle
  , drawCube
  , drawCylinder
  , drawSphere
  , rotate
  , scale
  , move
  , textureFill
  , colourFill
  , noFill
  , colourStroke
  , noStroke
  , setBackground
  , setAnimationStyle
  , pushScope
  , popScope
  )
where

import           Control.Monad.State.Strict
import           Lens.Simple                    ( use
                                                , assign
                                                , view
                                                )
import qualified Data.Map.Strict               as M

import           Data.Vec                       ( Mat44
                                                , multmm
                                                )

import           Graphics.Rendering.OpenGL
                                         hiding ( Cylinder
                                                , Fill
                                                , Line
                                                , Sphere
                                                , get
                                                , rotate
                                                , scale
                                                )

import           Gfx.EngineState               as ES
import           Gfx.GeometryBuffers
import           Gfx.Matrices
import           Gfx.Shaders
import           Gfx.Types                      ( Colour(..) )
import           Gfx.VertexBuffers              ( VBO
                                                , drawVBO
                                                )
import           Gfx.PostProcessing             ( AnimationStyle(..) )

import           ErrorHandling                  ( printErrors )

type GraphicsEngine v = StateT EngineState IO v

runGfx :: EngineState -> GraphicsEngine () -> IO EngineState
runGfx es action = execStateT action es

getFullMatrix :: GraphicsEngine (Mat44 GLfloat)
getFullMatrix = do
  mMat <- gets modelMatrix
  pMat <- use projectionMatrix
  vMat <- use viewMatrix
  return $ multmm (multmm pMat vMat) mMat

drawShape :: VBO -> GraphicsEngine ()
drawShape vbo = do
  mvp   <- getFullMatrix
  style <- gets currentFillStyle
  case style of
    ES.GFXFillColour fillC -> do
      program <- use colourShaders
      liftIO (currentProgram $= Just (shaderProgram program))
      lift $ setMVPMatrixUniform program mvp
      lift $ setColourUniform program fillC
      lift $ drawVBO vbo
    ES.GFXFillTexture name frame -> do
      program <- use textureShaders
      liftIO (currentProgram $= Just (shaderProgram program))
      textureLib <- use textureLibrary
      case M.lookup name textureLib >>= M.lookup frame of
        Nothing      -> return ()
        Just texture -> do
          lift $ activeTexture $= TextureUnit 0
          lift $ textureBinding Texture2D $= Just texture
          lift $ setMVPMatrixUniform program mvp
          lift $ drawVBO vbo
    ES.GFXNoFill -> return ()
  liftIO printErrors

drawWireframe :: VBO -> GraphicsEngine ()
drawWireframe vbo = do
  style <- gets currentStrokeStyle
  case style of
    ES.GFXStrokeColour strokeC -> do
      mvp     <- getFullMatrix
      program <- use colourShaders
      liftIO (currentProgram $= Just (shaderProgram program))
      lift $ setMVPMatrixUniform program mvp
      lift $ setColourUniform program strokeC
      lift $ drawVBO vbo
    ES.GFXNoStroke -> return ()
  liftIO printErrors

drawLine :: Float -> GraphicsEngine ()
drawLine l = do
  gbos <- use geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat l 1 1))
  drawWireframe (lineBuffer gbos)
  modify' popMatrix

drawRectangle :: Float -> Float -> GraphicsEngine ()
drawRectangle x y = do
  gbos <- use geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat x y 1))
  drawWireframe (rectWireBuffer gbos)
  drawShape (rectBuffer gbos)
  modify' popMatrix

drawCube :: Float -> Float -> Float -> GraphicsEngine ()
drawCube x y z = do
  gbos <- use geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat x y z))
  drawWireframe (cubeWireBuffer gbos)
  drawShape (cubeBuffer gbos)
  modify' popMatrix

drawCylinder :: Float -> Float -> Float -> GraphicsEngine ()
drawCylinder x y z = do
  gbos <- use geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat x y z))
  drawWireframe (cylinderWireBuffer gbos)
  drawShape (cylinderBuffer gbos)
  modify' popMatrix

drawSphere :: Float -> Float -> Float -> GraphicsEngine ()
drawSphere x y z = do
  gbos <- use geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat x y z))
  drawWireframe (sphereWireBuffer gbos)
  drawShape (sphereBuffer gbos)
  modify' popMatrix

rotate :: Float -> Float -> Float -> GraphicsEngine ()
rotate x y z = modify' (\es -> ES.multMatrix es $ rotMat x y z)

scale :: Float -> Float -> Float -> GraphicsEngine ()
scale x y z = modify' (\es -> ES.multMatrix es $ scaleMat x y z)

move :: Float -> Float -> Float -> GraphicsEngine ()
move x y z = modify' (\es -> ES.multMatrix es $ translateMat x y z)

setBackground :: Float -> Float -> Float -> GraphicsEngine ()
setBackground r g b = assign ES.backgroundColor (Colour r g b 1)

setAnimationStyle :: AnimationStyle -> GraphicsEngine ()
setAnimationStyle = assign ES.animationStyle

textureFill :: String -> Float -> GraphicsEngine ()
textureFill name frame =
  modify' (pushFillStyle $ ES.GFXFillTexture name (floor frame))

colourFill :: Float -> Float -> Float -> Float -> GraphicsEngine ()
colourFill r g b a =
  modify' (pushFillStyle $ ES.GFXFillColour $ Colour r g b a)

noFill :: GraphicsEngine ()
noFill = modify' (pushFillStyle ES.GFXNoFill)

colourStroke :: Float -> Float -> Float -> Float -> GraphicsEngine ()
colourStroke r g b a =
  modify' (pushStrokeStyle $ ES.GFXStrokeColour $ Colour r g b a)

noStroke :: GraphicsEngine ()
noStroke = modify' (pushStrokeStyle ES.GFXNoStroke)

pushScope :: GraphicsEngine ()
pushScope = do
  mStack  <- use matrixStack
  fStyles <- use fillStyles
  sStyles <- use strokeStyles
  stack   <- use scopeStack
  let savable = SavableState mStack fStyles sStyles
  assign scopeStack (savable : stack)

popScope :: GraphicsEngine ()
popScope = do
  stack <- use scopeStack
  let prev = head stack
  assign scopeStack   (tail stack)
  assign fillStyles   (view savedFillStyles prev)
  assign strokeStyles (view savedStrokeStyles prev)
  assign matrixStack  (view savedMatrixStack prev)
