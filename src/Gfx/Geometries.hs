module Gfx.Geometries where

import Data.List (genericIndex)
import Graphics.Rendering.OpenGL (Vertex3(..), GLfloat)

triVertexArray :: [Vertex3 GLfloat] -> [(Integer, Integer, Integer)] -> [Vertex3 GLfloat]
triVertexArray verts points =
  reverse $ foldl (\va (p1, p2, p3) -> genericIndex verts p1:genericIndex verts p2:genericIndex verts p3:va) [] points

lineVertexArray :: [Vertex3 GLfloat] -> [(Integer, Integer)] -> [Vertex3 GLfloat]
lineVertexArray verts points =
  reverse $ foldl (\va (p1, p2) -> genericIndex verts p1:genericIndex verts p2:va) [] points


cubeVertices :: GLfloat -> [Vertex3 GLfloat]
cubeVertices s = [
    (Vertex3 (-s) (-s) (-s)), (Vertex3 (-s) (-s) s),
    (Vertex3 s (-s) s),       (Vertex3 s (-s) (-s)),
    (Vertex3 (-s) s (-s)),    (Vertex3 (-s) s s),
    (Vertex3 s s s),          (Vertex3 s s (-s))
  ]

cubeTriangles :: [(Integer, Integer, Integer)]
cubeTriangles = [
    (0, 2, 1), (0, 3, 2),
    (0, 1, 5), (0, 5, 4),
    (1, 2, 6), (2, 6, 5),
    (2, 3, 7), (2, 7, 6),
    (3, 0, 4), (0, 4, 7),
    (4, 5, 6), (6, 7, 4)
  ]

cubeWireframe :: [(Integer, Integer)]
cubeWireframe = [
    (0, 1), (1, 2), (2, 3), (3, 0),
    (0, 4), (1, 5), (2, 6), (3, 7),
    (4, 5), (5, 6), (6, 7), (7, 0)
  ]

rectVertices :: GLfloat -> [Vertex3 GLfloat]
rectVertices s = [
    (Vertex3 (-s) (-s) 0), (Vertex3 s (-s) 0),
    (Vertex3 s s 0),       (Vertex3 (-s) s 0)
  ]

rectTriangles :: [(Integer, Integer, Integer)]
rectTriangles = [
    (0, 1, 2), (2, 3, 0)
  ]

rectWireframe :: [(Integer, Integer)]
rectWireframe = [
    (0, 1), (1, 2), (2, 3), (3, 0)
  ]

lineVertices :: GLfloat -> [Vertex3 GLfloat]
lineVertices s = [
    (Vertex3 0 (-s) 0), (Vertex3 0 s 0)
  ]

lineWireframe :: [(Integer, Integer)]
lineWireframe = [
    (0, 1)
  ]


{--
cylinderVertices :: GLfloat -> GLfloat -> Integer -> [(GLfloat, GLfloat, GLfloat)]
cylinderVertices height radius segments =
  let
    p = fromInteger <$> [0..segments-1]
    angDelta = (2.0 * pi) / fromInteger segments
    angles = fmap (angDelta *) p
    bottomCentre = (0, 0, 0)
    topCentre = (0, 0, height)
    bottomVerts = do
      a <- angles
      return (radius * sin a, radius * cos a, 0)
    topVerts = [(x, y, height) | (x, y, _) <- bottomVerts]
  in
    (bottomCentre : bottomVerts) ++ (topCentre : topVerts)

cylinderTriangles :: Integer -> [(Integer, Integer, Integer)]
cylinderTriangles segments =
  let
    bc = 0
    tc = segments + 1
    tOff = segments + 2
    bp = [1..segments]
    bTris = [(v, mod (v + 1) segments, bc) | v <- bp]
    tTris = fmap (\(x, y, _) -> (x + tOff, y + tOff, tc)) bTris
    sideTris = [(v, v + tc, mod (v + 1) segments) | v <- bp]

cylinderWireframe :: Integer -> [(Integer, Integer)]
cylinderWireframe segments = undefined

sphereVertices :: GLfloat -> Integer -> [(GLfloat, GLfloat, GLfloat)]
sphereVertices radius segments = undefined

sphereTriangles :: Integer -> [(Integer, Integer, Integer)]
sphereTriangles segments = undefined

sphereWireframes :: Integer -> [(Integer, Integer)]
sphereWireframes segments = undefined

--}
