module Gfx.EngineState where

import Graphics.Rendering.OpenGL (Color4(..))

data EngineState = EngineState {
    fillColours :: [ Color4 Double ]
  , strokeColours :: [ Color4 Double ]
  , drawTransparencies :: Bool
} deriving (Show, Eq)

baseState :: EngineState
baseState = EngineState {
    fillColours = [Color4 1 1 1 1]
  , strokeColours = [Color4 0 0 0 1]
  , drawTransparencies = False
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
