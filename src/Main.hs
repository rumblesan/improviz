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
  as <- readMVar appState
  case fst $ L.createGfx [("time", LA.Number (time as))] (validAst as) of
    Left msg -> putStrLn $ "Could not interpret program: " ++ msg
    Right scene ->
      do
        clearColor $= G.sceneBackground scene
        clear [ ColorBuffer, DepthBuffer ]
        loadIdentity
        evalStateT (G.interpretGfx $ G.sceneGfx scene) G.baseState
        loadIdentity
        evalStateT (G.interpretGfx $ G.sceneGfx scene) G.baseState { G.drawTransparencies = True }
        flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-2.5) 2.5 (-2.5 * hf/wf) (2.5 * hf/wf) (-10) 10
      else ortho (-2.5 * wf/hf) (2.5 * wf/hf) (-2.5) 2.5 (-10) 10
   matrixMode $= Modelview 0
   loadIdentity

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
