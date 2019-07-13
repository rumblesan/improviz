module Language.Interpreter.StdLib.Util
  ( addUtilStdLib
  )
where

import           Control.Monad.Except           ( throwError )

import           Language.Ast
import           Language.Interpreter           ( getExternal
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types
import           Language.Interpreter.Values

addUtilStdLib :: InterpreterProcess ()
addUtilStdLib = do
  setBuiltIn "isNull" isNullFunc
  setBuiltIn "ext"    getExtFunc

isNullFunc :: [Value] -> InterpreterProcess Value
isNullFunc args = case args of
  []      -> throwError "Need to provide isNull with argument"
  arg : _ -> return $ if valIsNull arg then Number 1 else Number 0

getExtFunc :: [Value] -> InterpreterProcess Value
getExtFunc args = case args of
  Symbol name : defaultValue : _ -> getExternal name defaultValue
  [Symbol name] -> getExternal name Null
  _ -> throwError "Need to provide ext with a name"
