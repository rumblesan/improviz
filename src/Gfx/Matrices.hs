module Gfx.Matrices where

import           Linear.Matrix                  ( M44 )
import           Linear.V3                      ( V3(..) )
import           Linear.V4                      ( V4(..) )
import           Linear.Epsilon                 ( Epsilon )
import           Linear.Projection              ( lookAt
                                                , perspective
                                                )

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
                 (V4 0 (cos f) (-sin f) 0)
                 (V4 0 (sin f) (cos f) 0)
                 (V4 0 0 0 1)

rotationY :: Floating f => f -> M44 f
rotationY f = V4 (V4 (cos f) 0 (sin f) 0)
                 (V4 0 1 0 0)
                 (V4 (-sin f) 0 (cos f) 0)
                 (V4 0 0 0 1)

rotationZ :: Floating f => f -> M44 f
rotationZ f = V4 (V4 (cos f) (-sin f) 0 0)
                 (V4 (sin f) (cos f) 0 0)
                 (V4 0 0 1 0)
                 (V4 0 0 0 1)

rotMat :: Floating f => f -> f -> f -> M44 f
rotMat xRot yRot zRot =
  let xRads = xRot
      yRads = yRot
      zRads = zRot
      sx    = sin xRads
      cx    = cos xRads
      sy    = sin yRads
      cy    = cos yRads
      sz    = sin zRads
      cz    = cos zRads
  in  V4 (V4 (cy * cz) (cy * (-sz)) sy 0)
         (V4 (sx * sy * cz + cx * sz) (cx * cz - sx * sy * sz) (-sx * cy) 0)
         (V4 (sx * sz - cx * sy * cz) (cx * sy * sz + sx * cz) (cx * cy) 0)
         (V4 0 0 0 1)

scaleMat :: Floating f => f -> f -> f -> M44 f
scaleMat xS yS zS = V4 (V4 xS 0 0 0) (V4 0 yS 0 0) (V4 0 0 zS 0) (V4 0 0 0 1)

translateMat :: Floating f => f -> f -> f -> M44 f
translateMat xT yT zT =
  V4 (V4 1 0 0 xT) (V4 0 1 0 yT) (V4 0 0 1 zT) (V4 0 0 0 1)
