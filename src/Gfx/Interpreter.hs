module Gfx.Interpreter where

import Data.Maybe (maybe)
import Control.Monad (mapM, when, void)
import Control.Monad.State.Strict

import Data.Vec (Mat44, multmm)

import Graphics.Rendering.OpenGL hiding (Fill, Line, get)

import Gfx.Ast
import Gfx.EngineState
import Gfx.GeometryBuffers
import Gfx.Shaders
import Gfx.Matrices

hasTransparency :: Color4 GLfloat -> Bool
hasTransparency (Color4 _ _ _ a) = a < 1.0

fullyTransparent :: Color4 GLfloat -> Bool
fullyTransparent (Color4 _ _ _ a) = a == 0

type GfxAction = IO
type GfxOutput = ()

type GraphicsEngine v = StateT EngineState GfxAction v

interpretGfx :: GfxAst -> GraphicsEngine GfxOutput
interpretGfx = void . interpretBlock

interpretBlock :: Block -> GraphicsEngine [ GfxOutput ]
interpretBlock = mapM interpretCommand

interpretCommand' :: GraphicsEngine GfxOutput -> Maybe Block -> GraphicsEngine GfxOutput
interpretCommand' commandOutput = maybe commandOutput (newScope commandOutput)


interpretCommand :: GfxCommand -> GraphicsEngine GfxOutput
interpretCommand (ShapeCommand shapeAst block) = interpretCommand' (interpretShape shapeAst) block
interpretCommand (MatrixCommand matrixAst block) = interpretCommand' (interpretMatrix matrixAst) block
interpretCommand (ColourCommand colourAst block) = interpretCommand' (interpretColour colourAst) block

getFullMatrix :: GraphicsEngine (Mat44 GLfloat)
getFullMatrix = do
  mMat <- gets modelMatrix
  pMat <- gets projectionMatrix
  vMat <- gets viewMatrix
  return $ multmm (multmm pMat vMat) mMat

drawShape :: VBO -> GraphicsEngine GfxOutput
drawShape (VBO bufferObject arrayIndex offset) = do
  fillC <- gets currentFillColour
  mvp <- getFullMatrix
  program <- gets shaders
  lift $ setMVPMatrixUniform program mvp
  lift $ setColourUniform program fillC
  bindVertexArrayObject $= Just bufferObject
  lift $ drawArrays Triangles arrayIndex offset

drawWireframe :: VBO -> GraphicsEngine GfxOutput
drawWireframe (VBO bufferObject arrayIndex offset) = do
  strokeC <- gets currentStrokeColour
  mvp <- getFullMatrix
  program <- gets shaders
  lift $ setMVPMatrixUniform program mvp
  lift $ setColourUniform program strokeC
  bindVertexArrayObject $= Just bufferObject
  lift $ drawArrays Lines arrayIndex offset

interpretShape :: ShapeGfx -> GraphicsEngine GfxOutput
interpretShape (Cube x y z) = do
  gbos <- gets geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat x y z))
  drawShape (cubeBuffer gbos)
  drawWireframe (cubeWireBuffer gbos)
  modify' popMatrix
interpretShape (Rectangle x y) = do
  gbos <- gets geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat x y 1))
  drawShape (rectBuffer gbos)
  drawWireframe (rectWireBuffer gbos)
  modify' popMatrix
interpretShape (Line l) = do
  gbos <- gets geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat l 1 1))
  drawWireframe (lineBuffer gbos)
  modify' popMatrix
interpretShape _ = undefined

interpretMatrix :: MatrixGfx -> GraphicsEngine GfxOutput
interpretMatrix (Rotate x y z) = modify' (\es -> pushMatrix es $ rotMat x y z)
interpretMatrix (Scale x y z) = modify' (\es -> pushMatrix es $ scaleMat x y z)
interpretMatrix (Move x y z) = modify' (\es -> pushMatrix es $ translateMat x y z)

interpretColour :: ColourGfx -> GraphicsEngine GfxOutput
interpretColour (Fill r g b a) = modify' (pushFillColour $ Color4 r g b a)
interpretColour NoFill = modify' (pushFillColour $ Color4 0 0 0 0)

interpretColour (Stroke r g b a) = modify' (pushStrokeColour $ Color4 r g b a)
interpretColour NoStroke = modify' (pushStrokeColour $ Color4 0 0 0 0)

newScope :: GraphicsEngine GfxOutput -> Block -> GraphicsEngine GfxOutput
newScope gfx block = do
  priorState <- get
  gfx
  _ <- interpretBlock block
  put priorState
