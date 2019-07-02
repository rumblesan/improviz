module Language.StdLib.Util
  ( addUtilStdLib
  )
where

import           Control.Monad.Except           ( throwError )

import           Language.Ast
import           Language.Interpreter           ( setBuiltIn )
import           Language.Interpreter.Types
import           Language.Interpreter.Values

addUtilStdLib :: InterpreterProcess ()
addUtilStdLib = setBuiltIn "isNull" isNullFunc

isNullFunc :: [Value] -> InterpreterProcess Value
isNullFunc args = case args of
  []      -> throwError "Need to provide isNull with argument"
  arg : _ -> return $ if valIsNull arg then Number 1 else Number 0
