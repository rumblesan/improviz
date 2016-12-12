module GfxInterpreter where

import Data.Map.Strict hiding (foldl)
import Data.Maybe (maybe)
import Control.Monad (mapM)
import Control.Monad.State.Strict

import Graphics.Rendering.OpenGL hiding (Fill)

import GfxAst
import Geometries


type GfxOutput = IO ()

data EngineState = EngineState {
  variables :: Map String Double
} deriving (Show, Eq)

type GraphicsEngine v = State EngineState v

interpretGfx :: GfxAst -> GraphicsEngine (IO ())
interpretGfx ast = do
  commands <- interpretBlock ast
  return $ sequence_ commands

interpretBlock :: Block -> GraphicsEngine [IO ()]
interpretBlock = mapM interpretCommand

interpretCommand' :: GraphicsEngine (IO ()) -> Maybe Block -> GraphicsEngine (IO ())
interpretCommand' commandOutput block =
  let
    blockMap :: Block -> GraphicsEngine (IO ())
    blockMap = newScope preservingMatrix commandOutput
  in
    maybe commandOutput blockMap block

interpretCommand :: GfxCommand -> GraphicsEngine GfxOutput
interpretCommand (ShapeCommand shapeAst block) = interpretCommand' (interpretShape shapeAst) block
interpretCommand (MatrixCommand matrixAst block) = interpretCommand' (interpretMatrix matrixAst) block
interpretCommand (ColourCommand colourAst block) = interpretCommand' (interpretColour colourAst) block


interpretShape :: ShapeGfx -> GraphicsEngine GfxOutput
interpretShape s@(Cube xV yV zV) =
  do
    x <- getValue xV
    y <- getValue yV
    z <- getValue zV
    return $ preservingMatrix $ do
      putStrLn $ "Interpreting " ++ show s
      scale x y z
      cube 1

interpretMatrix :: MatrixGfx -> GraphicsEngine GfxOutput
interpretMatrix m@(Rotate xV yV zV) =
  do
    x <- getValue xV
    y <- getValue yV
    z <- getValue zV
    return $ do
      putStrLn $ "Interpreting " ++ show m
      rotate x $ Vector3 1 0 0
      rotate y $ Vector3 0 1 0
      rotate z $ Vector3 0 0 1

interpretColour :: ColourGfx -> GraphicsEngine GfxOutput
interpretColour c@(Fill rV gV bV) =
  do
    r <- getValue rV
    g <- getValue gV
    b <- getValue bV
    return $ do
      putStrLn $ "Interpreting " ++ show c
      color $ Color3 (r/255) (g/255) (b/255)


newScope :: (IO () -> IO ()) -> GraphicsEngine (IO ()) -> Block -> GraphicsEngine GfxOutput
newScope preservingFunc newStateFunc block = do
  newStateIO <- newStateFunc
  blockIO <- interpretBlock block
  return $ do
    putStrLn "new scope"
    preservingFunc $ sequence_ (newStateIO : blockIO)

getValue :: Value -> GraphicsEngine Double
getValue (Number v) = return v
getValue (Variable name) = gets $ findWithDefault 0 name . variables
