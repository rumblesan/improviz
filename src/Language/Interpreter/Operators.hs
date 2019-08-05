module Language.Interpreter.Operators where

import           Control.Monad.Except           ( throwError )
import           Data.Fixed                     ( mod' )

import           Language.Ast                   ( Value(Number) )
import           Language.Interpreter.Types     ( InterpreterProcess )
import           Language.Interpreter.Values    ( getNumberValue )

binaryOp :: String -> Value -> Value -> InterpreterProcess Value
binaryOp op v1 v2 = do
  n1 <- getNumberValue v1
  n2 <- getNumberValue v2
  case op of
    "^"  -> return $ Number (n1 ** n2)
    "*"  -> return $ Number (n1 * n2)
    "/"  -> safeDiv n1 n2
    "+"  -> return $ Number (n1 + n2)
    "-"  -> return $ Number (n1 - n2)
    "%"  -> return $ Number (mod' n1 n2)
    "<"  -> return $ Number (if n1 < n2 then 1 else 0)
    ">"  -> return $ Number (if n1 > n2 then 1 else 0)
    "<=" -> return $ Number (if n1 <= n2 then 1 else 0)
    ">=" -> return $ Number (if n1 >= n2 then 1 else 0)
    "==" -> return $ Number (if n1 == n2 then 1 else 0)
    "!=" -> return $ Number (if n1 /= n2 then 1 else 0)
    "&&" -> return $ Number (if n1 /= 0 && n2 /= 0 then 1 else 0)
    "||" -> return $ Number (if n1 /= 0 || n2 /= 0 then 1 else 0)
    _    -> throwError $ "Unknown operator: " ++ op

unaryOp :: String -> Value -> InterpreterProcess Value
unaryOp op v = do
  n <- getNumberValue v
  case op of
    "-" -> return $ Number (-n)
    "!" -> return $ Number $ if n == 0 then 1 else 0
    _   -> throwError $ "Unknown operator: " ++ op

safeDiv :: Float -> Float -> InterpreterProcess Value
safeDiv n1 n2 = if n2 == 0
  then throwError "Cannot divide by zero"
  else return $ Number (n1 / n2)
