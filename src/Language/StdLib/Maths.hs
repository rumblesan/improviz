module Language.StdLib.Maths (
  sinFunc, cosFunc
) where


import Language.Interpreter.Types
import Language.Interpreter (getVariable)
import Language.Interpreter.Values
import Language.Ast

sinFunc :: InterpreterProcess Value
sinFunc = do
    rads <- getVariable "rads" >>= getNumberValue
    return $ Number $ sin rads

cosFunc :: InterpreterProcess Value
cosFunc = do
    rads <- getVariable "rads" >>= getNumberValue
    return $ Number $ cos rads

