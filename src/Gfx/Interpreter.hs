module Gfx.Interpreter
  ( interpretGfx
  )
where

import           Control.Monad                  ( mapM
                                                , void
                                                )
import           Control.Monad.State.Strict
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
                                                )

import           Gfx.Ast
import           Gfx.EngineState               as ES
import           Gfx.GeometryBuffers
import           Gfx.Matrices
import           Gfx.Shaders
import           Gfx.Types                      ( Colour(..) )
import           Gfx.VertexBuffers              ( VBO
                                                , drawVBO
                                                )

import           ErrorHandling                  ( printErrors )

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

interpretCommand :: GfxCommand -> GraphicsEngine GfxOutput
interpretCommand (ShapeCommand  shapeAst   ) = interpretShape shapeAst
interpretCommand (MatrixCommand matrixAst  ) = interpretMatrix matrixAst
interpretCommand (ColourCommand colourAst  ) = interpretColour colourAst
interpretCommand (ScopeCommand  instruction) = interpretScoping instruction

getFullMatrix :: GraphicsEngine (Mat44 GLfloat)
getFullMatrix = do
  mMat <- gets modelMatrix
  pMat <- gets projectionMatrix
  vMat <- gets viewMatrix
  return $ multmm (multmm pMat vMat) mMat

drawShape :: VBO -> GraphicsEngine GfxOutput
drawShape vbo = do
  mvp   <- getFullMatrix
  style <- gets currentFillStyle
  case style of
    ES.GFXFillColour fillC -> do
      program <- gets colourShaders
      liftIO (currentProgram $= Just (shaderProgram program))
      lift $ setMVPMatrixUniform program mvp
      lift $ setColourUniform program fillC
      lift $ drawVBO vbo
    ES.GFXFillTexture name frame -> do
      program <- gets textureShaders
      liftIO (currentProgram $= Just (shaderProgram program))
      textureLib <- gets textureLibrary
      case M.lookup name textureLib >>= M.lookup frame of
        Nothing      -> return ()
        Just texture -> do
          lift $ activeTexture $= TextureUnit 0
          lift $ textureBinding Texture2D $= Just texture
          lift $ setMVPMatrixUniform program mvp
          lift $ drawVBO vbo
    ES.GFXNoFill -> return ()
  liftIO printErrors

drawWireframe :: VBO -> GraphicsEngine GfxOutput
drawWireframe vbo = do
  style <- gets currentStrokeStyle
  case style of
    ES.GFXStrokeColour strokeC -> do
      mvp     <- getFullMatrix
      program <- gets colourShaders
      liftIO (currentProgram $= Just (shaderProgram program))
      lift $ setMVPMatrixUniform program mvp
      lift $ setColourUniform program strokeC
      lift $ drawVBO vbo
    ES.GFXNoStroke -> return ()
  liftIO printErrors

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
  modify' (pushFillStyle $ ES.GFXFillTexture name (floor frame))
interpretColour (Fill (ColourStyle r g b a)) =
  modify' (pushFillStyle $ ES.GFXFillColour $ Colour r g b a)
interpretColour NoFill = modify' (pushFillStyle ES.GFXNoFill)
interpretColour (Stroke r g b a) =
  modify' (pushStrokeStyle $ ES.GFXStrokeColour $ Colour r g b a)
interpretColour NoStroke = modify' (pushStrokeStyle ES.GFXNoStroke)

interpretScoping :: ScopeInstruction -> GraphicsEngine GfxOutput
interpretScoping PushScope = do
  st <- get
  let savable = SavableState { savedMatrixStack  = matrixStack st
                             , savedFillStyles   = fillStyles st
                             , savedStrokeStyles = strokeStyles st
                             }
  modify (\s -> s { scopeStack = savable : (scopeStack s) })
interpretScoping PopScope = modify popStack
 where
  popStack st =
    let prev = head $ scopeStack st
    in  st { scopeStack   = tail $ scopeStack st
           , fillStyles   = savedFillStyles prev
           , strokeStyles = savedStrokeStyles prev
           , matrixStack  = savedMatrixStack prev
           }
