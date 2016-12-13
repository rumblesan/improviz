module Main where

import Graphics.UI.GLUT hiding (Cube, get, Fill)

import Data.Map.Strict hiding (size)
import Control.Monad.State.Strict

import GfxAst
import GfxInterpreter

import Data.IORef

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
   where n' = fromIntegral n

displayAst :: GfxAst
displayAst = [ColourCommand (Fill (Number 1) (Number 0) (Number 0.4)) Nothing,
  MatrixCommand (Rotate (Variable "time") (Number 2) (Number 1)) $ Just [
    ShapeCommand (Cube (Number 0.1) (Number 0.2) (Number 0.1)) $ Just [
      ColourCommand (Fill (Number 0) (Number 0.7) (Number 0.2)) Nothing,
      MatrixCommand (Rotate (Number 2) (Variable "time") (Variable "time")) Nothing,
      ShapeCommand (Cube (Number 0.1) (Number 0.2) (Number 0.1)) Nothing
    ],
    MatrixCommand (Rotate (Variable "time") (Variable "time") (Number 0.3)) Nothing,
    ShapeCommand (Cube (Number 0.1) (Number 0.3) (Number 0.05)) Nothing
    ]]
startState :: EngineState
startState = EngineState {
    variables=fromList [("time", 1)]
  , fillColours=[Color3 1.0 1.0 1.0]
  , strokeColours=[Color3 0.0 0.0 0.0]
}

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer]
  _window <- createWindow _progName
  reshapeCallback $= Just reshape
  depthFunc $= Just Less
  initialState <- newIORef startState
  displayCallback $= display initialState
  idleCallback $= Just (idle initialState)
  mainLoop

display :: IORef EngineState -> DisplayCallback
display engineState = do
  clear [ ColorBuffer, DepthBuffer ]
  clear [ ColorBuffer ]
  loadIdentity
  es <- readIORef engineState
  putStrLn "display loop"
  evalStateT (interpretGfx displayAst) es
  flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing


idle :: IORef EngineState -> IdleCallback
idle engineState = do
  es <- readIORef engineState
  let newvars = adjust (1 +) "time" (variables es)
  writeIORef engineState es {variables=newvars}
  postRedisplay Nothing
