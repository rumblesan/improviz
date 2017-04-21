module Language.Interpreter.Values where

import Control.Monad.Except

import Language.Ast
import Language.Interpreter.Types

getValueType :: Value -> String
getValueType (Number _) = "Number"
getValueType Null = "Null"
getValueType (Lambda _ _) = "Lambda"
getValueType (BuiltIn _) = "BuiltIn"

getNumberValue :: Value -> InterpreterProcess Float
getNumberValue (Number v) = return v
getNumberValue v = throwError ("Expected number but got " ++ getValueType v)
