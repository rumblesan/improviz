module Language.Interpreter.Operators where

import Control.Monad.Except
import Data.Fixed (mod')

import Language.Ast
import Language.Interpreter.Types
import Language.Interpreter.Values

binaryOp :: String -> Value -> Value -> InterpreterProcess Value
binaryOp op v1 v2 = do
  n1 <- getNumberValue v1
  n2 <- getNumberValue v2
  case op of
    "^" -> return $ Number (n1 ** n2)
    "*" -> return $ Number (n1 * n2)
    "/" -> return $ Number (n1 / n2)
    "+" -> return $ Number (n1 + n2)
    "-" -> return $ Number (n1 - n2)
    "%" -> return $ Number (mod' n1 n2)
    _   -> throwError $ "Unknown operator: " ++ op

unaryOp :: String -> Value -> InterpreterProcess Value
unaryOp op v = do
  n <- getNumberValue v
  case op of
    "-" -> return $ Number (-n)
    _   -> throwError $ "Unknown operator: " ++ op
