module Gfx.Interpreter where

import           Control.Monad              (mapM, void, when)
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (maybe)

import           Data.Vec                   (Mat44, multmm)

import           Graphics.Rendering.OpenGL  hiding (Cylinder, Fill, Line,
                                             Sphere, get)

import           Gfx.Ast
import           Gfx.EngineState            as ES
import           Gfx.GeometryBuffers
import           Gfx.Matrices
import           Gfx.Shaders
import           Gfx.VertexBuffers          (VBO, drawVBO)

import           ErrorHandling              (printErrors)

hasTransparency :: Color4 GLfloat -> Bool
hasTransparency (Color4 _ _ _ a) = a < 1.0

fullyTransparent :: Color4 GLfloat -> Bool
fullyTransparent (Color4 _ _ _ a) = a == 0

type GfxAction = IO

type GfxOutput = ()

type GraphicsEngine v = StateT EngineState GfxAction v

interpretGfx :: GfxAst -> GraphicsEngine GfxOutput
interpretGfx ast = do
  program <- gets colourShaders
  liftIO (currentProgram $= Just (shaderProgram program))
  void $ interpretBlock ast

interpretBlock :: Block -> GraphicsEngine [GfxOutput]
interpretBlock = mapM interpretCommand

interpretCommand' ::
     GraphicsEngine GfxOutput -> Maybe Block -> GraphicsEngine GfxOutput
interpretCommand' commandOutput = maybe commandOutput (newScope commandOutput)

interpretCommand :: GfxCommand -> GraphicsEngine GfxOutput
interpretCommand (ShapeCommand shapeAst block) =
  interpretCommand' (interpretShape shapeAst) block
interpretCommand (MatrixCommand matrixAst block) =
  interpretCommand' (interpretMatrix matrixAst) block
interpretCommand (ColourCommand colourAst block) =
  interpretCommand' (interpretColour colourAst) block

getFullMatrix :: GraphicsEngine (Mat44 GLfloat)
getFullMatrix = do
  mMat <- gets modelMatrix
  pMat <- gets projectionMatrix
  vMat <- gets viewMatrix
  return $ multmm (multmm pMat vMat) mMat

drawShape :: VBO -> GraphicsEngine GfxOutput
drawShape vbo = do
  mvp <- getFullMatrix
  style <- gets currentFillStyle
  case style of
    ES.GFXColour fillC -> do
      program <- gets colourShaders
      liftIO (currentProgram $= Just (shaderProgram program))
      lift $ setMVPMatrixUniform program mvp
      lift $ setColourUniform program fillC
      lift $ drawVBO vbo
    ES.GFXTexture name frame -> do
      program <- gets textureShaders
      liftIO (currentProgram $= Just (shaderProgram program))
      textureLib <- gets textureLibrary
      case M.lookup (name, frame) textureLib of
        Nothing -> return ()
        Just texture -> do
          lift $ activeTexture $= TextureUnit 0
          lift $ textureBinding Texture2D $= Just texture
          lift $ setMVPMatrixUniform program mvp
          lift $ drawVBO vbo
  liftIO printErrors

drawWireframe :: VBO -> GraphicsEngine GfxOutput
drawWireframe vbo = do
  strokeC <- gets currentStrokeStyle
  mvp <- getFullMatrix
  program <- gets colourShaders
  liftIO (currentProgram $= Just (shaderProgram program))
  lift $ setMVPMatrixUniform program mvp
  lift $ setColourUniform program strokeC
  lift $ drawVBO vbo

interpretShape :: ShapeGfx -> GraphicsEngine GfxOutput
interpretShape (Cube x y z) = do
  gbos <- gets geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat x y z))
  drawWireframe (cubeWireBuffer gbos)
  drawShape (cubeBuffer gbos)
  modify' popMatrix
interpretShape (Rectangle x y) = do
  gbos <- gets geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat x y 1))
  drawWireframe (rectWireBuffer gbos)
  drawShape (rectBuffer gbos)
  modify' popMatrix
interpretShape (Line l) = do
  gbos <- gets geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat l 1 1))
  drawWireframe (lineBuffer gbos)
  modify' popMatrix
interpretShape (Cylinder x y z) = do
  gbos <- gets geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat x y z))
  drawWireframe (cylinderWireBuffer gbos)
  drawShape (cylinderBuffer gbos)
  modify' popMatrix
interpretShape (Sphere x y z) = do
  gbos <- gets geometryBuffers
  modify' (\es -> pushMatrix es (scaleMat x y z))
  drawWireframe (sphereWireBuffer gbos)
  drawShape (sphereBuffer gbos)
  modify' popMatrix

interpretMatrix :: MatrixGfx -> GraphicsEngine GfxOutput
interpretMatrix (Rotate x y z) =
  modify' (\es -> ES.multMatrix es $ rotMat x y z)
interpretMatrix (Scale x y z) =
  modify' (\es -> ES.multMatrix es $ scaleMat x y z)
interpretMatrix (Move x y z) =
  modify' (\es -> ES.multMatrix es $ translateMat x y z)

interpretColour :: ColourGfx -> GraphicsEngine GfxOutput
interpretColour (Fill (TextureStyle name frame)) =
  modify' (pushFillStyle $ ES.GFXTexture name (floor frame))
interpretColour (Fill (ColourStyle r g b a)) =
  modify' (pushFillStyle $ ES.GFXColour $ Color4 r g b a)
interpretColour NoFill = modify' (pushFillStyle $ ES.GFXColour $ Color4 0 0 0 0)
interpretColour (Stroke r g b a) = modify' (pushStrokeStyle $ Color4 r g b a)
interpretColour NoStroke = modify' (pushStrokeStyle $ Color4 0 0 0 0)

newScope :: GraphicsEngine GfxOutput -> Block -> GraphicsEngine GfxOutput
newScope gfx block = do
  priorState <- get
  gfx
  _ <- interpretBlock block
  put priorState
