module Gfx.EngineState where

import Graphics.Rendering.OpenGL (Color4(..))

import Gfx.Ast (Block)
import Gfx.GeometryBuffers (GeometryBuffers, createAllBuffers)

data Scene = Scene {
  sceneBackground :: Color4 Float,
  sceneGfx :: Block
}

data EngineState = EngineState {
    fillColours :: [ Color4 Double ]
  , strokeColours :: [ Color4 Double ]
  , drawTransparencies :: Bool
  , geometryBuffers :: GeometryBuffers
} deriving (Show, Eq)

baseState :: IO EngineState
baseState = do
  gbos <- createAllBuffers
  return EngineState {
    fillColours = [Color4 1 1 1 1]
  , strokeColours = [Color4 0 0 0 1]
  , drawTransparencies = False
  , geometryBuffers = gbos
}


pushFillColour :: Color4 Double -> EngineState -> EngineState
pushFillColour c es = es { fillColours = c : fillColours es }

currentFillColour :: EngineState -> Color4 Double
currentFillColour = head . fillColours

popFillColour :: EngineState -> EngineState
popFillColour es = es { fillColours = tail $ fillColours es }

pushStrokeColour :: Color4 Double -> EngineState -> EngineState
pushStrokeColour c es = es { strokeColours = c : strokeColours es }

currentStrokeColour :: EngineState -> Color4 Double
currentStrokeColour = head . strokeColours

popStrokeColour :: EngineState -> EngineState
popStrokeColour es = es { strokeColours = tail $ strokeColours es }
