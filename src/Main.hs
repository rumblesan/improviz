module Main where

import Graphics.UI.GLUT

import Control.Monad

import Geometries

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
   where n' = fromIntegral n


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow _progName
  displayCallback $= display
  reshapeCallback $= Just reshape
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  forM_ (points 7) $ \(x, y, z) ->
    preservingMatrix $ do
      translate $ Vector3 x y z
      cube 0.1
  flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing
