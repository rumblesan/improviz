module Improviz.SystemVars where

import           Lens.Simple                    ( (^.) )

import           Linear.Matrix                  ( (!*!)
                                                , (!*)
                                                , inv44
                                                )
import           Linear.V3                      ( V3(..) )
import           Linear.V4                      ( V4(..)
                                                , point
                                                , _x
                                                , _y
                                                )

import           Gfx.Engine                     ( GfxEngine
                                                , projectionMatrix
                                                , viewMatrix
                                                )
import           Language.Ast                   ( Value(..) )

create :: GfxEngine -> [(String, Value)]
create gfx =
  let pMatrix          = gfx ^. projectionMatrix
      vMatrix          = gfx ^. viewMatrix
      iPVMatrix        = inv44 (pMatrix !*! vMatrix)
      V4 _ _ zVal wVal = pMatrix !* (vMatrix !* point (V3 0.0 0.0 0.0))
      screenCorner     = V4 (1.0 * wVal) (1.0 * wVal) zVal wVal
      cornerPos        = iPVMatrix !* screenCorner
      cornerX          = cornerPos ^. _x
      cornerY          = cornerPos ^. _y
      aspect           = cornerX / cornerY
  in  [ ("aspect"       , Number aspect)
      , ("screenCornerX", Number cornerX)
      , ("screenCornerY", Number cornerY)
      ]
