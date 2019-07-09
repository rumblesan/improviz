module Language.ImpVM.StdLib.BlockHandling
  ( blockBuiltIns
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Lens.Simple                    ( use )

import           Language.ImpVM.Types           ( VM
                                                , StackItem(..)
                                                , externalState
                                                )

import           Gfx.Context                    ( GfxContext(..) )

blockBuiltIns :: [(String, [StackItem] -> VM GfxContext ())]
blockBuiltIns = [("pushScope", pushGfxScope), ("popScope", popGfxScope)]

pushGfxScope :: [StackItem] -> VM GfxContext ()
pushGfxScope _ = do
  ctx <- use externalState
  liftIO $ pushScope ctx

popGfxScope :: [StackItem] -> VM GfxContext ()
popGfxScope _ = do
  ctx <- use externalState
  liftIO $ popScope ctx
