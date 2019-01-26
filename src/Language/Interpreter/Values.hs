module Language.Interpreter.Values where

import           Control.Monad.Except

import           Language.Ast
import           Language.Interpreter.Types

getValueType :: Value -> String
getValueType (Number _)                = "Number"
getValueType Null                      = "Null"
getValueType (Symbol _)                = "Symbol"
getValueType (UserFunctionRef name)    = "Function: " ++ name
getValueType (BuiltInFunctionRef name) = "BuiltIn: " ++ name

getNumberValue :: Value -> InterpreterProcess Float
getNumberValue (Number v) = return v
getNumberValue v = throwError ("Expected number but got " ++ getValueType v)
