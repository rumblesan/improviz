module Language.StdLib.CollectionOps
  ( addCollectionStdLib
  ) where

import           Control.Monad.Except

import           Language.Ast               (Value (Number, VList))
import           Language.Interpreter       (getVarOrNull, interpretExpression,
                                             setBuiltIn)
import           Language.Interpreter.Types (BuiltInFunction,
                                             InterpreterProcess)

addCollectionStdLib :: InterpreterProcess ()
addCollectionStdLib = setBuiltIn "elem" listElem ["list", "index"]

listElem :: BuiltInFunction
listElem = do
  list <- getVarOrNull "list"
  index <- getVarOrNull "index"
  case (list, index) of
    (VList elems, Number idx) -> do
      let i = round idx
      if length elems < i || i < 0
        then throwError "Index out of range"
        else interpretExpression $ elems !! i
    _ -> throwError "elem needs a list and a number"
