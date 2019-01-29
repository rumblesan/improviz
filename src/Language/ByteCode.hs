module Language.ByteCode where

import           Data.Maybe
import           Control.Monad
import           Control.Monad.Trans.State.Strict

import           Language.Ast
import           Language.Interpreter.Scope

data ByteOp
  = Push Float
  | Pop
  | AbsJump Integer
  | RelJump Integer
  | Branch Integer
  | GreaterThan
  | Equal
  | LessThan
  | BinaryOp String
  | UnaryOp String
  | Load Integer
  | Save Integer
  | Constant Float
  | CallFunction Integer
  | Address Integer
  | NullOp
  | SymbolOp String
  | ReturnOp


data TransformState
  { nextAddress       :: Int
  , variableAddresses :: ScopeStack String Int
  }

type ByteCodeCompiler = State TransformState

programToBC :: Program -> ByteCodeCompiler [ByteOp]
programToBC (Program statements) = mapM statementToBC statements

statementToBC :: Statement -> ByteCodeCompiler [ByteOp]
statementToBC (StLoop loop)         = loopToBC loop
statementToBC (StAssign assignment) = assignmentToBC assignment
statementToBC (StExpression expr)   = expressionToBC expr
statementToBC (StIf ifSt)           = ifToBC ifSt
statementToBC (StFunc func)         = funcToBC func

blockToBC :: Block -> ByteCodeCompiler [ByteOp]
blockToBC (Block elements) = mapM elementToBC elements

elementToBC :: Element -> ByteCodeCompiler [ByteOp]
elementToBC (ElLoop loop)         = loopToBC loop
elementToBC (ElAssign assignment) = assignmentToBC assignment
elementToBC (ElExpression expr)   = expressionToBC expr
elementToBC (ElIf ifSt)           = ifToBC ifSt

loopToBC :: Loop -> ByteCodeCompiler [ByteOp]
loopToBC loop = undefined

assignmentToBC :: Assignment -> ByteCodeCompiler [ByteOp]
assignmentToBC (AbsoluteAssignment name expr)    = do
  exprOps <- expressionToBC expr
  mbAddr <- getAddress name
  addr <- case addr of
    Null -> assignAddress name
    Address a -> a
  return undefined

assignmentToBC (ConditionalAssignment name expr) = undefined

ifToBC :: If -> [ByteOp]
ifToBC (If predicate ifBlock elseBlock) = do
  predOps <- expressionToBC predicate
  ifOps <- blockToBC ifBlock
  elseOps <- blockToBC $ fromMaybe [] elseBlock
  return $ predOps ++ [Branch $ (length elseOps) + 1] ++ elseOps ++ [RelJump (length ifOps) + 1] ++ ifOps


funcToBC :: Func -> ByteCodeCompiler [ByteOp]
funcToBC (Func name args block) = do
  mbAddr <- getAddress name
  addr <- case mbAddr of
    Null -> assignAddress name
    Address a -> a
  return undefined

expressionToBC :: Expression -> ByteCodeCompiler [ByteOp]
expressionToBC (EApp application)        = applicationToBC application
expressionToBC (BinaryOp op expr1 expr2) = do
  expr1Ops <- expressionToBC expr1
  expr2Ops <- expressionToBC expr2
  return $ expr1Ops ++ expr2Ops ++ [(BinaryOp op)]
expressionToBC (UnaryOp op expr)         = do
  exprOps <- expressionToBC expr
  return $ exprOps ++ [(UnaryOp op)]
expressionToBC (EVar var)                = variableToBC var
expressionToBC (EVal val)                = valueToBC val

variableToBC :: Variable -> ByteCodeCompiler [ByteOp]
variableToBC (LocalVariable name) = getAddress name
variableToBC (GlobalVariable name) = getAddress name

valueToBC :: Value -> ByteCodeCompiler [ByteOp]
valueToBC (Number n)                = return [Constant n]
valueToBC Null                      = return [NullOp]
valueToBC (Symbol v)                = return [SymbolOp v]
valueToBC (UserFunctionRef name)    = getAddress name
valueToBC (BuiltInFunctionRef name) = getAddress name

getAddress :: String -> ByteCodeCompiler [ByteOp]
getAddress name = gets (\st -> [maybe Null Address (getVariable (variableAddresses st) name)])

assignAddress :: String -> ByteCodeCompiler Integer
assignAddress name = do
  st <- get
  let addr = nextAddress st
  let stack = variableAddresses st
  puts st { nextAddress = addr + 1, variableAddresses = setVariable stack name addr }
  return addr



