module Gfx.EngineState where

import Graphics.Rendering.OpenGL (Color4(..), GLfloat)
import Data.Vec (Mat44)

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
