module Gfx.Geometries where

cubeVertices :: Float -> [(Float, Float, Float)]
cubeVertices s = [
    (-s, -s, -s), (-s, -s, s), (s, -s, s), (s, -s, -s),
    (-s, s, -s), (-s, s, s), (s, s, s), (s, s, -s)
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

rectVertices :: Float -> [(Float, Float, Float)]
rectVertices s = [
    (-s, -s, 0), (s, -s, 0),
    (s, s, 0), (-s, s, 0)
  ]

rectTriangles :: [(Integer, Integer, Integer)]
rectTriangles = [
    (0, 1, 2), (2, 3, 0)
  ]

rectWireframe :: [(Integer, Integer)]
rectWireframe = [
    (0, 1), (1, 2), (2, 3), (3, 0)
  ]

lineVertices :: Float -> [(Float, Float, Float)]
lineVertices s = [
    (0, -s, 0), (0, s, 0)
  ]

lineWireframe :: [(Integer, Integer)]
lineWireframe = [
    (0, 1)
  ]


{--
cylinderVertices :: Float -> Float -> Integer -> [(Float, Float, Float)]
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

sphereVertices :: Float -> Integer -> [(Float, Float, Float)]
sphereVertices radius segments = undefined

sphereTriangles :: Integer -> [(Integer, Integer, Integer)]
sphereTriangles segments = undefined

sphereWireframes :: Integer -> [(Integer, Integer)]
sphereWireframes segments = undefined

--}
