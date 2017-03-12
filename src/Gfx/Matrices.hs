{-# LANGUAGE RankNTypes #-}  
{-# LANGUAGE TypeOperators #-}

module Gfx.Matrices where

import Data.Vec

vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x :. y :. z:. ()

projectionMat :: Floating f => f -> f -> f -> f -> Mat44 f
projectionMat near far fov aspect = perspective near far fov aspect

viewMat :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> Mat44 a
viewMat = lookAt

lookAt :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> Mat44 a
lookAt eye target up = x :. y :. z :. h :. ()
  where
    forward = normalize $ target - eye
    right = normalize $ cross forward up
    up' = cross right forward
    x = snoc right (-(dot right eye))
    y = snoc up' (-(dot up' eye))
    z = snoc (-forward) (dot forward eye)
    h = 0 :. 0 :. 0 :. 1 :. ()

rotMat :: Float -> Float -> Float -> Mat44 Float
rotMat xRot yRot zRot = rotationEuler $ xRot :. yRot :. zRot :. ()
