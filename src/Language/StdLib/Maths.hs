module Language.StdLib.Maths (
  sinFunc, cosFunc
) where


import Language.Interpreter.Types
import Language.Interpreter (getVariable)
import Language.Interpreter.Values
import Language.LanguageAst

sinFunc :: Maybe Block -> InterpreterProcess Value
sinFunc _ = do
    rads <- getVariable "rads" >>= getNumberValue
    return $ Number $ sin rads

cosFunc :: Maybe Block -> InterpreterProcess Value
cosFunc _ = do
    rads <- getVariable "rads" >>= getNumberValue
    return $ Number $ cos rads

