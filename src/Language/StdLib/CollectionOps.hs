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
addCollectionStdLib = do
  setBuiltIn "elem" listElem ["list", "index"]
  setBuiltIn "count" listCount ["list"]

listElem :: BuiltInFunction
listElem = do
  list <- getVarOrNull "list"
  index <- getVarOrNull "index"
  case (list, index) of
    (VList elems, Number idx) -> do
      let i = floor idx
      if length elems < i || i < 0
        then throwError "Index out of range"
        else interpretExpression $ elems !! i
    _ -> throwError "elem needs a list and a number"

listCount :: BuiltInFunction
listCount = do
  list <- getVarOrNull "list"
  case list of
    (VList elems) -> return $ Number $ fromIntegral $ length elems
    _             -> throwError "count needs a list"
