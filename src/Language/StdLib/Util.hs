module Language.StdLib.Util
  ( addUtilStdLib
  )
where

import           Language.Ast
import           Language.Interpreter           ( getVariable
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types
import           Language.Interpreter.Values

addUtilStdLib :: InterpreterProcess ()
addUtilStdLib = do
  setBuiltIn "isNull" isNullFunc [VarArg "v"]

isNullFunc :: InterpreterProcess Value
isNullFunc = do
  v <- getVariable "v"
  return $ if valIsNull v then Number 1 else Number 0
