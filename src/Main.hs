module Main where

import Graphics.UI.GLUT hiding (Cube, Fill)

import Data.Time.Clock.POSIX

import Control.Monad.State.Strict (evalStateT)
import Control.Concurrent

import qualified Gfx as G
import qualified Language as L
import qualified Language.LanguageAst as LA
import AppServer
import AppTypes

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  get glutVersion >>= print
  scrSize <- get screenSize
  --initialWindowSize $= scrSize
  print scrSize
  initialDisplayMode $= [WithDepthBuffer, Borderless, Captionless]
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
  as <- readMVar appState
  case fst $ L.createGfx [("time", LA.Number (time as))] (validAst as) of
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
        return as { time = newTime }
