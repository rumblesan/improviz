module Gfx.Matrices where

import           Linear.Matrix                  ( M44 )
import           Linear.V3                      ( V3(..) )
import           Linear.V4                      ( V4(..) )
import           Linear.Epsilon                 ( Epsilon )
import           Linear.Projection              ( lookAt
                                                , perspective
                                                )

toRads :: Floating f => f
toRads = pi / 180.0

viewMat :: (Floating f, Num f, Epsilon f) => V3 f -> V3 f -> V3 f -> M44 f
viewMat = lookAt

projectionMat :: Floating f => f -> f -> f -> f -> M44 f
projectionMat near far fov aspect = perspective fov aspect near far

orthographicMat :: Floating f => f -> f -> f -> f -> f -> f -> M44 f
orthographicMat left right top bottom near far = V4
  (V4 (2.0 / (right - left)) 0 0 0)
  (V4 0 (2.0 / (top - bottom)) 0 0)
  (V4 0 0 (2.0 / (far - near)) ((-(far + near)) / (far - near)))
  (V4 0 0 0 1)

rotationX :: Floating f => f -> M44 f
rotationX f = V4 (V4 1 0 0 0)
                 (V4 0 (cos (f * toRads)) (-sin (f * toRads)) 0)
                 (V4 0 (sin (f * toRads)) (cos (f * toRads)) 0)
                 (V4 0 0 0 1)

rotationY :: Floating f => f -> M44 f
rotationY f = V4 (V4 (cos (f * toRads)) 0 (sin (f * toRads)) 0)
                 (V4 0 1 0 0)
                 (V4 (-sin (f * toRads)) 0 (cos (f * toRads)) 0)
                 (V4 0 0 0 1)

rotationZ :: Floating f => f -> M44 f
rotationZ f = V4 (V4 (cos (f * toRads)) (-sin (f * toRads)) 0 0)
                 (V4 (sin (f * toRads)) (cos (f * toRads)) 0 0)
                 (V4 0 0 1 0)
                 (V4 0 0 0 1)

--rotMat :: Floating f => f -> f -> f -> M44 f
--rotMat xRot yRot zRot = rotationZ zRot !*! rotationY yRot !*! rotationX xRot

rotMat :: Floating f => f -> f -> f -> M44 f
rotMat xRot yRot zRot =
  let xRads = xRot * toRads
      yRads = yRot * toRads
      zRads = zRot * toRads
      sx    = sin xRads
      cx    = cos xRads
      sy    = sin yRads
      cy    = cos yRads
      sz    = sin zRads
      cz    = cos zRads
  in  V4 (V4 (cx * cy) (cx * sy * sz - sx * cz) (cx * sy * cz + sx * sz) 0)
         (V4 (sx * cy) (sx * sy * sz + cx * cz) (sx * sy * cz - cx * sz) 0)
         (V4 (-sy) (cy * sz) (cy * cz) 0)
         (V4 0 0 0 1)

scaleMat :: Floating f => f -> f -> f -> M44 f
scaleMat xS yS zS = V4 (V4 xS 0 0 0) (V4 0 yS 0 0) (V4 0 0 zS 0) (V4 0 0 0 1)

translateMat :: Floating f => f -> f -> f -> M44 f
translateMat xT yT zT =
  V4 (V4 1 0 0 xT) (V4 0 1 0 yT) (V4 0 0 1 zT) (V4 0 0 0 1)
