module Gfx.GfxEngineState where

import Graphics.Rendering.OpenGL (Color4, GLfloat)

data EngineState = EngineState {
    fillColours :: [ Color4 Double ]
  , strokeColours :: [ Color4 Double ]
  , backgroundColour :: Color4 GLfloat
  , drawTransparencies :: Bool
} deriving (Show, Eq)

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

setBackgroundColour :: Color4 GLfloat -> EngineState -> EngineState
setBackgroundColour c es = es { backgroundColour = c }
