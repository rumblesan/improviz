module Main where

import Graphics.UI.GLUT hiding (Cube, get, Fill)

import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict

import qualified Gfx as G
import qualified Language as L
import qualified Language.LanguageAst as LA

import Data.IORef

program :: String
program = "fill(0.5 0 1);\nstroke(0 0 0);\nrotate(time);\nbox(0.2 0.5 0.5);\n"

startTime :: Double
startTime = 0

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer]
  _window <- createWindow _progName
  reshapeCallback $= Just reshape
  depthFunc $= Just Less
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  time <- newIORef startTime
  displayCallback $= display time
  idleCallback $= Just (idle time)
  mainLoop

display :: IORef Double -> DisplayCallback
display time = do
  putStrLn "display loop"
  t <- readIORef time

  parsed <- case L.parse program of
    Nothing -> do
      putStrLn "Could not parse program"
      return $ LA.Block []
    Just p -> return p
  case fst $ L.createGfx [("time", LA.Number t)] parsed of
    Left msg -> putStrLn $ "Could not interpret program: " ++ msg
    Right gfx ->
      do
        clearColor $= G.backgroundColour G.baseState
        clear [ ColorBuffer, DepthBuffer ]
        loadIdentity
        evalStateT (G.interpretGfx gfx) G.baseState
        loadIdentity
        evalStateT (G.interpretGfx gfx) G.baseState { G.drawTransparencies = True }
        flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing


idle :: IORef Double -> IdleCallback
idle time = do
  t <- readIORef time
  writeIORef time (t + 0.1)
  postRedisplay Nothing
