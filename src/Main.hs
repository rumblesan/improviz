module Main where

import Graphics.UI.GLUT hiding (Cube, get, Fill)

import Data.Time.Clock.POSIX

import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict

import qualified Gfx as G
import qualified Language as L
import qualified Language.LanguageAst as LA

import Data.IORef

programText :: String
programText = "fill(0.5 0 1);\nstroke(0 0 0);\nrotate(time);\nbox(0.2 0.5 0.5);\n"

startTime :: Double
startTime = 0

data AppState = AppState {
  time :: Double,
  program :: String,
  validAst :: LA.Block,
  timeAtStart :: Double
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
  timeNow <- realToFrac <$> getPOSIXTime
  appState <- newIORef AppState {
    time = 0, program = programText, validAst = LA.Block [], timeAtStart = timeNow
  }
  displayCallback $= display appState
  idleCallback $= Just (idle appState)
  mainLoop

display :: IORef AppState -> DisplayCallback
display appState = do
  putStrLn "display loop"
  as <- readIORef appState

  ast <- if program as /= "" then
    case L.parse programText of
      Nothing -> do
        putStrLn "Could not parse program"
        return $ LA.Block []
      Just newAst -> do
        writeIORef appState as { program = "", validAst = newAst }
        return newAst
   else
    return $ validAst as

  case fst $ L.createGfx [("time", LA.Number (time as))] ast of
    Left msg -> putStrLn $ "Could not interpret program: " ++ msg
    Right gfx ->
      do
        putStrLn "Interpreting program"
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


idle :: IORef AppState -> IdleCallback
idle appState = do
  t <- readIORef appState
  timeNow <- realToFrac <$> getPOSIXTime
  let newTime = timeNow - timeAtStart t
  putStrLn $ "New time " ++ show newTime
  writeIORef appState t { time = newTime }
  postRedisplay Nothing
