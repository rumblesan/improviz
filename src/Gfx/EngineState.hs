module Gfx.EngineState where

import Graphics.Rendering.OpenGL (Color4(..), GLfloat)
import Data.Vec (Mat44, multmm, identity)

import Gfx.Ast (Block)
import Gfx.GeometryBuffers (GeometryBuffers, createAllBuffers)
import Gfx.Shaders

data Scene = Scene {
  sceneBackground :: Color4 Float,
  sceneGfx :: Block
}

data EngineState = EngineState {
    fillColours :: [ Color4 GLfloat ]
  , strokeColours :: [ Color4 GLfloat ]
  , drawTransparencies :: Bool
  , geometryBuffers :: GeometryBuffers
  , shaders :: Shaders
  , viewMatrix :: Mat44 GLfloat
  , projectionMatrix :: Mat44 GLfloat
  , matrixStack :: [ Mat44 GLfloat ]
} deriving (Show, Eq)

baseState :: Mat44 GLfloat -> Mat44 GLfloat -> IO EngineState
baseState projection view = do
  gbos <- createAllBuffers
  shd <- createShaders
  return EngineState {
    fillColours = [Color4 1 1 1 1]
  , strokeColours = [Color4 0 0 0 1]
  , drawTransparencies = False
  , geometryBuffers = gbos
  , shaders = shd
  , viewMatrix = view
  , projectionMatrix = projection
  , matrixStack = [identity]
}


pushFillColour :: Color4 GLfloat -> EngineState -> EngineState
pushFillColour c es = es { fillColours = c : fillColours es }

currentFillColour :: EngineState -> Color4 GLfloat
currentFillColour = head . fillColours

popFillColour :: EngineState -> EngineState
popFillColour es = es { fillColours = tail $ fillColours es }

pushStrokeColour :: Color4 GLfloat -> EngineState -> EngineState
pushStrokeColour c es = es { strokeColours = c : strokeColours es }

currentStrokeColour :: EngineState -> Color4 GLfloat
currentStrokeColour = head . strokeColours

popStrokeColour :: EngineState -> EngineState
popStrokeColour es = es { strokeColours = tail $ strokeColours es }

pushMatrix :: EngineState -> Mat44 Float -> EngineState
pushMatrix es mat =
  let
    stack = matrixStack es
    comp = multmm (head stack) mat
  in
    es { matrixStack = comp : stack }

popMatrix :: EngineState -> EngineState
popMatrix es = es { matrixStack = tail $ matrixStack es }

modelMatrix :: EngineState -> Mat44 Float
modelMatrix = head . matrixStack

multMatrix :: EngineState -> Mat44 Float -> EngineState
multMatrix es mat =
  let
    stack = matrixStack es
    newhead = multmm (head stack) mat
  in
    es { matrixStack = newhead : tail stack }

dupeHeadMatrix :: EngineState -> EngineState
dupeHeadMatrix es =
  es { matrixStack = head (matrixStack es) : matrixStack es }
