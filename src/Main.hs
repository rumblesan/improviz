module Main where

import Graphics.UI.GLUT

import Control.Monad

import Geometries

import Data.IORef

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
   where n' = fromIntegral n


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow _progName
  angle <- newIORef 0.0
  displayCallback $= display angle
  reshapeCallback $= Just reshape
  idleCallback $= Just (idle angle)
  mainLoop

display :: IORef GLfloat -> DisplayCallback
display angle = do
  clear [ ColorBuffer ]
  loadIdentity
  a <- get angle
  rotate a $ Vector3 0 0 1
  forM_ (points 7) $ \(x, y, z) ->
    preservingMatrix $ do
      translate $ Vector3 x y z
      cube 0.1
  flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing


idle :: IORef GLfloat -> IdleCallback
idle angle = do
  angle $~! (+ 0.1)
  postRedisplay Nothing
