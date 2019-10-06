module Language.Interpreter.Values
  ( getNumberValue
  , getValueType
  , valIsNull
  )
where

import           Control.Monad.Except

import           Language.Ast
import           Language.Interpreter.Types

getValueType :: Value -> String
getValueType (Number _)                = "Number"
getValueType Null                      = "Null"
getValueType (Symbol             _   ) = "Symbol"
getValueType (BuiltInFunctionRef name) = "BuiltIn: " ++ name

valIsNull :: Value -> Bool
valIsNull Null = True
valIsNull _    = False

getNumberValue :: Value -> InterpreterProcess Float
getNumberValue (Number v) = return v
getNumberValue v = throwError ("Expected number but got " ++ getValueType v)
