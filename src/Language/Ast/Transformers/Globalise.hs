module Language.Ast.Transformers.Globalise
  ( globalise
  )
where

import           Control.Monad.Trans.State.Strict
import           Data.Set                       ( Set(..) )
import qualified Data.Set                      as S

import           Language.Ast

newtype InliningState = InliningState
  { globalVars :: Set String
  }

type Transformer = State InliningState

localState :: (s -> s) -> State s a -> State s a
localState f ls = gets (evalState ls . f)

globalise :: Set String -> Program -> Program
globalise globals p = evalState (globaliseProgram p) (InliningState globals)

globaliseProgram :: Program -> Transformer Program
globaliseProgram (Program statements) =
  Program <$> mapM globaliseStatement statements

globaliseStatement :: Statement -> Transformer Statement
globaliseStatement (StLoop loop) = StLoop <$> globaliseLoop loop
globaliseStatement (StAssign assignment) =
  StAssign <$> globaliseAssignment assignment
globaliseStatement (StExpression expression) =
  StExpression <$> globaliseExpression expression
globaliseStatement (StIf   ifAst) = StIf <$> globaliseIf ifAst
globaliseStatement (StFunc func ) = StFunc <$> globaliseFunc func

globaliseBlock :: Set String -> Block -> Transformer Block
globaliseBlock newVars (Block elements) = Block <$> localState
  (\st -> st { globalVars = S.difference (globalVars st) newVars })
  (mapM globaliseElement elements)

globaliseElement :: Element -> Transformer Element
globaliseElement (ElLoop loop) = ElLoop <$> globaliseLoop loop
globaliseElement (ElAssign assignment) =
  ElAssign <$> globaliseAssignment assignment
globaliseElement (ElExpression expression) =
  ElExpression <$> globaliseExpression expression
globaliseElement (ElIf ifAst) = ElIf <$> globaliseIf ifAst

globaliseLoop :: Loop -> Transformer Loop
globaliseLoop (Loop expr mbId block) =
  Loop
    <$> globaliseExpression expr
    <*> pure mbId
    <*> globaliseBlock (maybe S.empty S.singleton mbId) block

globaliseAssignment :: Assignment -> Transformer Assignment
globaliseAssignment (AbsoluteAssignment ident expr) = do
  newExpr <- globaliseExpression expr
  modify
    (\st -> st { globalVars = S.difference (globalVars st) (S.singleton ident) }
    )
  return $ AbsoluteAssignment ident newExpr
globaliseAssignment (ConditionalAssignment ident expr) = do
  newExpr <- globaliseExpression expr
  modify
    (\st -> st { globalVars = S.difference (globalVars st) (S.singleton ident) }
    )
  return $ ConditionalAssignment ident newExpr

globaliseExpression :: Expression -> Transformer Expression
globaliseExpression (EApp application) =
  EApp <$> globaliseApplication application
globaliseExpression (BinaryOp op expr1 expr2) =
  BinaryOp op <$> globaliseExpression expr1 <*> globaliseExpression expr2
globaliseExpression (UnaryOp op expr1) =
  UnaryOp op <$> globaliseExpression expr1
globaliseExpression (EVar variable) = EVar <$> globaliseVariable variable
globaliseExpression (EVal value   ) = EVal <$> globaliseValue value

globaliseIf :: If -> Transformer If
globaliseIf (If predicate block elseBlock) =
  If
    <$> globaliseExpression predicate
    <*> globaliseBlock S.empty block
    <*> mapM (globaliseBlock S.empty) elseBlock

globaliseFunc :: Func -> Transformer Func
globaliseFunc (Func name args block) = do
  modify (\st -> st { globalVars = S.insert name (globalVars st) })
  Func name args <$> globaliseBlock (argNames args) block

argNames :: [FuncArg] -> S.Set String
argNames args = S.fromList $ an <$> args
 where
  an (VarArg   n) = n
  an (BlockArg n) = n

globaliseApplication :: Application -> Transformer Application
globaliseApplication (Application name args mbBlock) =
  Application
    <$> globaliseVariable name
    <*> mapM globaliseExpression      args
    <*> mapM (globaliseBlock S.empty) mbBlock

globaliseVariable :: Variable -> Transformer Variable
globaliseVariable v@(LocalVariable name) =
  gets (\st -> if S.member name (globalVars st) then GlobalVariable name else v)
globaliseVariable globalVar = return globalVar

globaliseValue :: Value -> Transformer Value
globaliseValue = return
