module Main where

import Graphics.UI.GLUT hiding (Cube, Fill)

import Data.Time.Clock.POSIX

import Control.Monad.State.Strict (evalStateT)
import Control.Concurrent

import qualified Gfx as G
import qualified Gfx.Pipeline as GP
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
  gfxState <- G.baseState
  shaders <- GP.initShaders
  appState <- makeAppState >>= newMVar
  _ <- forkIO $ runServer appState
  displayCallback $= display gfxState appState shaders
  idleCallback $= Just (idle appState)
  mainLoop

display :: G.EngineState -> MVar AppState -> GP.Shaders -> DisplayCallback
display gfxState appState shaders = do
  as <- readMVar appState
  case fst $ L.createGfx [("time", LA.Number (time as))] (validAst as) of
    Left msg -> putStrLn $ "Could not interpret program: " ++ msg
    Right scene ->
      do
        clearColor $= G.sceneBackground scene
        clear [ ColorBuffer, DepthBuffer ]
        currentProgram $= Just (GP.program shaders)
        let ic = Vector4 0.5 1 0.1 1 :: Vector4 GLfloat
        uniform (GP.inputColourU shaders) $= ic
        loadIdentity
        evalStateT (G.interpretGfx $ G.sceneGfx scene) gfxState
        loadIdentity
        evalStateT (G.interpretGfx $ G.sceneGfx scene) gfxState { G.drawTransparencies = True }
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
