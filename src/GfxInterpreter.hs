module GfxInterpreter where

import Data.Map.Strict hiding (foldl)
import Data.Maybe (maybe)
import Control.Monad (mapM)
import Control.Monad.State.Strict

import Graphics.Rendering.OpenGL hiding (Fill)

import GfxAst
import Geometries


type GfxAction = IO
type GfxOutput = ()

data EngineState = EngineState {
  variables :: Map String Double
} deriving (Show, Eq)

type GraphicsEngine v = StateT EngineState GfxAction v

interpretGfx :: GfxAst -> GraphicsEngine GfxOutput
interpretGfx ast = do
  _ <- interpretBlock ast
  return ()

interpretBlock :: Block -> GraphicsEngine [ GfxOutput ]
interpretBlock = mapM interpretCommand

interpretCommand' :: GraphicsEngine GfxOutput -> Maybe Block -> GraphicsEngine GfxOutput
interpretCommand' commandOutput = maybe commandOutput (newScope commandOutput)


interpretCommand :: GfxCommand -> GraphicsEngine GfxOutput
interpretCommand (ShapeCommand shapeAst block) = interpretCommand' (interpretShape shapeAst) block
interpretCommand (MatrixCommand matrixAst block) = interpretCommand' (interpretMatrix matrixAst) block
interpretCommand (ColourCommand colourAst block) = interpretCommand' (interpretColour colourAst) block

interpretShape :: ShapeGfx -> GraphicsEngine GfxOutput
interpretShape (Cube xV yV zV) =
  do
    x <- getValue xV
    y <- getValue yV
    z <- getValue zV
    lift $ preservingMatrix $ do
      scale x y z
      cube 1

interpretMatrix :: MatrixGfx -> GraphicsEngine GfxOutput
interpretMatrix (Rotate xV yV zV) =
  do
    x <- getValue xV
    y <- getValue yV
    z <- getValue zV
    lift $ rotate x $ Vector3 1 0 0
    lift $ rotate y $ Vector3 0 1 0
    lift $ rotate z $ Vector3 0 0 1

interpretColour :: ColourGfx -> GraphicsEngine GfxOutput
interpretColour (Fill rV gV bV) =
  do
    r <- getValue rV
    g <- getValue gV
    b <- getValue bV
    lift $ color $ Color3 (r/255) (g/255) (b/255)


getValue :: Value -> GraphicsEngine Double
getValue (Number v) = return v
getValue (Variable name) = gets $ findWithDefault 0 name . variables

newScope :: GraphicsEngine GfxOutput -> Block -> GraphicsEngine GfxOutput
newScope gfx block =
  let
    stateMap :: IO (a, EngineState) -> IO (GfxOutput, EngineState)
    stateMap d = preservingMatrix $ do
      (_, engineState) <- d
      _ <- evalStateT (interpretBlock block) engineState
      return ((), engineState)
  in
    mapStateT stateMap gfx
