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
    Vertex3 (-s) (-s) (-s), Vertex3 (-s) (-s) s,
    Vertex3 s (-s) s,       Vertex3 s (-s) (-s),
    Vertex3 (-s) s (-s),    Vertex3 (-s) s s,
    Vertex3 s s s,          Vertex3 s s (-s)
  ]

cubeTriangles :: [(Integer, Integer, Integer)]
cubeTriangles = [
    (0, 2, 1), (0, 3, 2),
    (0, 1, 5), (0, 5, 4),
    (1, 2, 6), (2, 6, 5),
    (2, 3, 7), (2, 7, 6),
    (3, 0, 4), (3, 4, 7),
    (4, 5, 6), (6, 7, 4)
  ]

cubeWireframe :: [(Integer, Integer)]
cubeWireframe = [
    (0, 1), (1, 2), (2, 3), (3, 0),
    (0, 4), (1, 5), (2, 6), (3, 7),
    (4, 5), (5, 6), (6, 7), (7, 4)
  ]

rectVertices :: GLfloat -> [Vertex3 GLfloat]
rectVertices s = [
    Vertex3 (-s) (-s) 0, Vertex3 s (-s) 0,
    Vertex3 s s 0,       Vertex3 (-s) s 0
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
    Vertex3 0 (-s) 0, Vertex3 0 s 0
  ]

lineWireframe :: [(Integer, Integer)]
lineWireframe = [
    (0, 1)
  ]


cylinderVertices :: GLfloat -> GLfloat -> Integer -> [Vertex3 GLfloat]
cylinderVertices height radius segments =
  let
    p = fromInteger <$> [0..segments-1]
    angDelta = (2.0 * pi) / fromInteger segments
    angles = fmap (angDelta *) p
    bottomCentre = Vertex3 0 0 0
    topCentre = Vertex3 0 0 height
    bottomVerts = do
      a <- angles
      return (Vertex3 (radius * sin a) (radius * cos a) 0)
    topVerts = [Vertex3 x y height | (Vertex3 x y _) <- bottomVerts]
  in
    (bottomCentre : bottomVerts) ++ (topCentre : topVerts)

cylinderTriangles :: Integer -> [(Integer, Integer, Integer)]
cylinderTriangles segments =
  let
    bc = 0
    tc = segments + 1
    bp = [1..segments]
    bTris = [(v, mod v segments + 1, bc) | v <- bp]
    tTris = fmap (\(x, y, c) -> (x + tc, y + tc, c + tc)) bTris
    sideLowerTris = [(v, v + tc, mod v segments + tc + 1) | v <- bp]
    sideUpperTris = [(v, mod v segments + tc + 1, mod v segments + 1)| v <- bp]
  in
    bTris ++ sideLowerTris ++ sideUpperTris ++ tTris

cylinderWireframe :: Integer -> [(Integer, Integer)]
cylinderWireframe segments =
  let
    tc = segments + 1
    bc = 0
    bp = [1..segments]
    bEdgeLines = [(v, mod v segments + 1) | v <- bp]
    bCentreLines = [(bc, v) | v <- bp]
    tEdgeLines = fmap (\(x, y) -> (x + tc, y + tc)) bEdgeLines
    tCentreLines = [(tc, v + tc) | v <- bp]
    sideLines = [(v, v + tc) | v <- bp]
  in
    bEdgeLines ++ bCentreLines ++ sideLines ++ tEdgeLines ++ tCentreLines

sphereVertices :: GLfloat -> Integer -> [Vertex3 GLfloat]
sphereVertices radius segments =
  let
    angDelta = (2.0 * pi) / fromInteger segments
    bottomCentre = Vertex3 0 0 (-radius) :: Vertex3 GLfloat
    topCentre = Vertex3 0 0 radius :: Vertex3 GLfloat
    rings = div (segments - 1) 2
    allRings = do
      ring <- [1..rings]
      ringVerts radius angDelta (fromInteger ring) segments
  in
    topCentre : allRings ++ [bottomCentre]
  where
    ringVerts :: GLfloat -> GLfloat -> GLfloat -> Integer -> [Vertex3 GLfloat]
    ringVerts radius angle heightRing segments =
      let
        width = sin (heightRing * angle) * radius
        height = cos (heightRing * angle) * radius
        points = fromInteger <$> [0..segments-1]
      in fmap (\p -> Vertex3 (sin (p * angle) * width) (cos (p * angle) * width) height ) points


sphereTriangles :: Integer -> [(Integer, Integer, Integer)]
sphereTriangles segments =
  let
    rings = div (segments-1) 2
    topCentre = 0
    bottomCentre = (segments * rings) + 1

    topTriangles = do
      s <- [1..segments]
      return (s, mod s segments + 1, topCentre)

    bottomTriangles = do
      s <- [1..segments]
      let offset = segments * (rings - 1)
      return (s + offset, mod s segments + offset + 1, bottomCentre)

    intraRingTriangles = do
      r <- [1..(rings-1)]
      s <- [1..segments]
      let o = (r-1) * segments
      let upper = (o + s, mod s segments + 1 + o, (mod s segments + 1) + (r * segments))
      let lower = (o + s, (mod s segments + 1) + (r * segments), o + s + segments)
      [upper, lower]
  in
    topTriangles ++ intraRingTriangles ++ bottomTriangles

sphereWireframe :: Integer -> [(Integer, Integer)]
sphereWireframe segments =
  let
    rings = div (segments-1) 2
    topCentre = 0
    bottomCentre = segments * rings + 1
    topLines = fmap (\s -> (s, 0)) [1..segments]
    ringLines = do
      r <- [0..(rings-1)]
      let offset = r * segments
      s <- [1..segments]
      return (s + offset, mod s segments + offset + 1)
    intraRingLines = do
      r <- [0..(rings-2)]
      s <- [1..segments]
      return (s + r * segments, s + (r + 1) * segments)
    bottomLines = fmap (\s -> (s + ((rings - 1) * segments), bottomCentre)) [1..segments]
  in
    topLines ++ ringLines ++ intraRingLines ++ bottomLines

