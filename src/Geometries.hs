module Geometries where

import Graphics.UI.GLUT

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

cubeVertexes :: GLfloat -> [(GLfloat, GLfloat, GLfloat)]
cubeVertexes w = [
  ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
  ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
  ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
  (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
  ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
  ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f $ cubeVertexes w
