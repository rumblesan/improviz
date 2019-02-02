{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Gfx.Matrices where

import           Data.Vec

vec3 :: forall a a1 a2 . a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x :. y :. z :. ()

projectionMat :: Floating f => f -> f -> f -> f -> Mat44 f
projectionMat near far fov aspect = perspective near far fov aspect

orthographicMat :: Floating f => f -> f -> f -> f -> f -> f -> Mat44 f
orthographicMat left right top bottom near far = matFromList
  [ 2.0 / (right - left)
  , 0
  , 0
  , 0
  , 0
  , 2.0 / (top - bottom)
  , 0
  , 0
  , 0
  , 0
  , 2.0 / (far - near)
  , (-(far + near)) / (far - near)
  , 0
  , 0
  , 0
  , 1
  ]

viewMat :: Floating f => Vec3 f -> Vec3 f -> Vec3 f -> Mat44 f
viewMat = lookAt

lookAt :: Floating f => Vec3 f -> Vec3 f -> Vec3 f -> Mat44 f
lookAt eye target up = x :. y :. z :. h :. ()
 where
  forward = normalize $ target - eye
  right   = normalize $ cross forward up
  up'     = cross right forward
  x       = snoc right (-(dot right eye))
  y       = snoc up' (-(dot up' eye))
  z       = snoc (-forward) (dot forward eye)
  h       = 0 :. 0 :. 0 :. 1 :. ()

rotMat :: Floating f => f -> f -> f -> Mat44 f
rotMat xRot yRot zRot = rotationEuler $ xRot :. yRot :. zRot :. ()

scaleMat :: Floating f => f -> f -> f -> Mat44 f
scaleMat xS yS zS = scaling $ vec3 xS yS zS

translateMat :: Floating f => f -> f -> f -> Mat44 f
translateMat xT yT zT = translation $ vec3 xT yT zT
