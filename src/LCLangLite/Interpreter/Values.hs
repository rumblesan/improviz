module LCLangLite.Interpreter.Values where

import Control.Monad.Except

import LCLangLite.LanguageAst
import LCLangLite.Interpreter.Types

getValueType :: Value -> String
getValueType (Number _) = "Number"
getValueType Null = "Null"
getValueType (Lambda _ _) = "Lambda"
getValueType (BuiltIn _) = "BuiltIn"

getNumberValue :: Value -> InterpreterProcess Double
getNumberValue (Number v) = return v
getNumberValue v = throwError ("Expected number but got " ++ getValueType v)
