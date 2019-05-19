module Language.ImpVM.StdLib where

import qualified Data.Map                      as M
import           Control.Monad.IO.Class         ( liftIO )
import           Lens.Simple                    ( assign )

import           Language.ImpVM.Types
import           Language.ImpVM.VM

addStdLib :: VM ()
addStdLib = assign builtins $ M.fromList [("print", printValue)]

printValue :: VM ()
printValue = do
  v <- popStack
  liftIO $ print v
