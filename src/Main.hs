module Main where

import Graphics.UI.GLUT hiding (Cube, get, Fill)

import Data.Time.Clock.POSIX

import Control.Monad.State.Strict
import Control.Concurrent

import qualified Gfx as G
import qualified Language as L
import qualified Language.LanguageAst as LA
import AppServer
import AppTypes

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer]
  _window <- createWindow _progName
  reshapeCallback $= Just reshape
  depthFunc $= Just Less
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  appState <- makeAppState >>= newMVar
  _ <- forkIO $ runServer appState
  displayCallback $= display appState
  idleCallback $= Just (idle appState)
  mainLoop

display :: MVar AppState -> DisplayCallback
display appState = do
  putStrLn "display loop"
  as <- readMVar appState
  case fst $ L.createGfx [("time", LA.Number (time as))] (validAst as) of
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


idle :: MVar AppState -> IdleCallback
idle appState =
  do
    modifyMVar_ appState incrementTime
    postRedisplay Nothing
  where
    incrementTime as =
      do
        timeNow <- realToFrac <$> getPOSIXTime
        let newTime = 10 * (timeNow - timeAtStart as)
        putStrLn $ "New time " ++ show newTime
        return as { time = newTime }
