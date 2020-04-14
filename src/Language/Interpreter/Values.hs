module Language.Interpreter.Values
  ( getNumberValue
  , getNumberAsRads
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

toRads :: Float
toRads = pi / 180.0

getNumberAsRads :: Value -> InterpreterProcess Float
getNumberAsRads (Number v) = return $ v * toRads
getNumberAsRads v = throwError ("Expected number but got " ++ getValueType v)
