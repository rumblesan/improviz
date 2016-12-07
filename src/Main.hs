module Main where

import Graphics.UI.GLUT

import Geometries

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
  cube 0.1
  flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing
