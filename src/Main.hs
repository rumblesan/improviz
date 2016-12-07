import Graphics.UI.GLUT

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

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
  renderPrimitive Triangles $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing
