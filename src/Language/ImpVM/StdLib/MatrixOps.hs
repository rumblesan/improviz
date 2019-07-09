module Language.ImpVM.StdLib.MatrixOps
  ( gfxMatrix
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Lens.Simple                    ( use )

import           Language.ImpVM.Types           ( VM
                                                , StackItem(..)
                                                , externalState
                                                )
import           Language.ImpVM.VM              ( setError )

import           Gfx.Context                    ( GfxContext(..) )

gfxMatrix :: [StackItem] -> VM GfxContext ()
gfxMatrix args = do
  ctx <- use externalState
  case args of
    [SString name, SFloat x, SFloat y, SFloat z] -> case name of
      "rotate" -> liftIO $ rotate ctx x y z
      "scale"  -> liftIO $ scale ctx x y z
      "move"   -> liftIO $ move ctx x y z
      n        -> setError $ "unrecognised matrix (" ++ n ++ ")"
    _ -> setError "invalid arguments given to matrix"
