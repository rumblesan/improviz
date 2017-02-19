module Main where

import Graphics.UI.GLUT hiding (Cube, get, Fill)

import Data.Map.Strict hiding (size)
import Control.Monad.State.Strict

import Gfx

import Data.IORef

displayAst :: GfxAst
displayAst = [
  ColourCommand (Stroke 0 0 0 1) Nothing,
  ColourCommand NoFill Nothing,
  MatrixCommand (Rotate 0 0 1) $ Just [
    ShapeCommand (Cube 0.3 0.6 0.2) $ Just [
      ColourCommand (Fill 0 0.7 0.2 0.5) Nothing,
      MatrixCommand (Rotate 2 3 3) Nothing,
      MatrixCommand (Move 1 0.3 0) Nothing,
      ShapeCommand (Cube 0.3 0.5 0.1) Nothing
    ],
    ColourCommand (Fill 1 0 0.2 0) Nothing,
    MatrixCommand (Rotate 3 3 0.3) Nothing,
    ShapeCommand (Cube 0.4 0.3 0.5) Nothing
    ]]

startState :: EngineState
startState = EngineState {
    fillColours = [Color4 1 1 1 1]
  , strokeColours = [Color4 0 0 0 1]
  , backgroundColour = Color4 1 1 1 1
  , drawTransparencies = False
}

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer]
  _window <- createWindow _progName
  reshapeCallback $= Just reshape
  depthFunc $= Just Less
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  initialState <- newIORef startState
  displayCallback $= display initialState
  idleCallback $= Just (idle initialState)
  mainLoop

display :: IORef EngineState -> DisplayCallback
display engineState = do
  putStrLn "display loop"
  es <- readIORef engineState
  clearColor $= backgroundColour es
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  evalStateT (interpretGfx displayAst) es
  loadIdentity
  evalStateT (interpretGfx displayAst) es { drawTransparencies = True }
  flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing


idle :: IORef EngineState -> IdleCallback
idle engineState = do
  es <- readIORef engineState
  postRedisplay Nothing
