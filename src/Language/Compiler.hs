module Language.Compiler where

import           Data.Vector                    ( Vector )
import qualified Data.List                     as L
import qualified Data.Vector                   as V
-- import           Data.Maybe                     ( fromMaybe )
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Except           ( runExceptT
                                                , throwError
                                                )

import           Language.ImpVM.Types           ( Instruction(..)
                                                , Op(..)
                                                , StackItem(..)
                                                )
import           Language.Ast

import           Language.Compiler.State

opToOperator :: String -> Maybe Op
opToOperator "+" = Just AddOp
opToOperator "-" = Just SubOp
opToOperator "*" = Just MultOp
opToOperator "/" = Just DivOp
opToOperator _   = Nothing

track :: ImpCompiler (Vector Instruction) -> ImpCompiler (Vector Instruction)
track work = do
  bc <- work
  incrementPosition $ V.length bc
  return bc

compile :: Program -> Either String (Vector Instruction)
compile program = evalState (runExceptT (astToBC program)) emptyTransformState

pp :: Vector Instruction -> IO ()
pp bc =
  let indexed = zip [0 ..] (V.toList bc)
      printed = fmap pb indexed
  in  mapM_ putStrLn printed
  where pb (idx, b) = show idx ++ ": " ++ show b

astToBC :: Program -> ImpCompiler (Vector Instruction)
astToBC (Program statements) = do
  bc <- V.concat <$> mapM statementToBC statements
  return $ V.snoc bc End

statementToBC :: Statement -> ImpCompiler (Vector Instruction)
statementToBC (StLoop       loop      ) = track $ loopToBC loop
statementToBC (StAssign     assignment) = track $ assignmentToBC assignment
statementToBC (StExpression expr      ) = track $ expressionToBC expr
statementToBC (StIf         ifSt      ) = track $ ifToBC ifSt
statementToBC (StFunc       func      ) = track $ funcToBC func

blockToBC :: Block -> ImpCompiler (Vector Instruction)
blockToBC (Block elements) = V.concat <$> mapM elementToBC elements

elementToBC :: Element -> ImpCompiler (Vector Instruction)
elementToBC (ElLoop       loop      ) = track $ loopToBC loop
elementToBC (ElAssign     assignment) = track $ assignmentToBC assignment
elementToBC (ElExpression expr      ) = track $ expressionToBC expr
elementToBC (ElIf         ifSt      ) = track $ ifToBC ifSt

loopToBC :: Loop -> ImpCompiler (Vector Instruction)
loopToBC (Loop expr mbVar block) = do
  pushVariableScope
  loopAddr    <- assignAnonAddress
  loopVarAddr <- case mbVar of
    Just blockVarName -> assignAddress blockVarName
    Nothing           -> assignAnonAddress
  exprOps <- expressionToBC expr
  let loopExprBC    = V.snoc exprOps (Save loopAddr)
  let loopVarInitBC = V.fromList [Constant (SFloat 0), Save loopVarAddr]
  blockOps <- blockToBC block
  let loopCheckBC = V.fromList
        [ Load loopAddr
        , Load loopVarAddr
        , Operator EQOp
        , RelJump (V.length blockOps + 5)
        ]
  let loopVarIncrBC = V.fromList
        [ Load loopVarAddr
        , Constant (SFloat 1)
        , Operator AddOp
        , Save loopVarAddr
        ]
  let
    loopResetBC = V.singleton
      (RelJump
        ( (-1)
        * (V.length loopVarIncrBC + V.length blockOps + V.length loopCheckBC)
        )
      )
  popVariableScope
  return
    $    loopExprBC
    V.++ loopVarInitBC
    V.++ loopCheckBC
    V.++ blockOps
    V.++ loopVarIncrBC
    V.++ loopResetBC

assignmentToBC :: Assignment -> ImpCompiler (Vector Instruction)
assignmentToBC (AbsoluteAssignment name expr) = do
  exprOps <- expressionToBC expr
  mbAddr  <- getAddress name
  addr    <- case mbAddr of
    Nothing -> assignAddress name
    Just a  -> return a
  return $ V.snoc exprOps (Save addr)
assignmentToBC (ConditionalAssignment name expr) = do
  exprOps <- expressionToBC expr
  mbAddr  <- getAddress name
  addr    <- case mbAddr of
    Nothing -> assignAddress name
    Just a  -> return a
  let assignment = V.snoc exprOps (Save addr)
  let comp = V.fromList
        [ Load addr
        , Constant SNull
        , Operator NEQOp
        , Branch (V.length assignment + 1)
        ]
  return $ comp V.++ assignment

simpleAssignmentBC :: String -> Expression -> ImpCompiler (Vector Instruction)
simpleAssignmentBC name expr = do
  exprOps <- expressionToBC expr
  mbAddr  <- getAddress name
  addr    <- case mbAddr of
    Nothing -> assignAddress name
    Just a  -> return a
  return $ V.snoc exprOps (Save addr)

ifToBC :: If -> ImpCompiler (Vector Instruction)
ifToBC (If predBlocks) = undefined
  {--
  predOps <- expressionToBC predicate
  ifOps   <- blockToBC ifBlock
  elseOps <- blockToBC $ fromMaybe (Block []) elseBlock
  return
    $    V.snoc predOps (Branch $ V.length elseOps + 1)
    V.++ V.snoc elseOps (RelJump $ V.length ifOps + 1)
    V.++ ifOps
    --}


funcToBC :: Func -> ImpCompiler (Vector Instruction)
funcToBC (Func name args block) = do
  incrementPosition 1 -- increment over the Jump Op
  markFunction name
  pushVariableScope
  argBC   <- V.concat <$> mapM funcArgToBC args
  blockBC <- blockToBC block
  let ret    = V.singleton Return
  let bodyBC = argBC V.++ blockBC V.++ ret
  let jmp    = RelJump (V.length bodyBC + 1)
  popVariableScope
  return $ V.cons jmp bodyBC

funcArgToBC :: FuncArg -> ImpCompiler (Vector Instruction)
funcArgToBC (VarArg name _) = do
  addr <- assignAddress name
  return $ V.singleton $ Save addr
funcArgToBC (BlockArg name) = throwError "Block args not yet supported"

applicationToBC :: Application -> ImpCompiler (Vector Instruction)
applicationToBC (Application name args block) = do
  argBC  <- V.concat <$> mapM applicationArgToBC (L.reverse args)
  callOp <- case name of
    LocalVariable n -> do
      addr <- functionAddress n
      return $ Call addr (length args)
    GlobalVariable n -> return $ BuiltIn n (length args)
  return $ V.snoc argBC callOp

applicationArgToBC :: ApplicationArg -> ImpCompiler (Vector Instruction)
applicationArgToBC (ApplicationSingleArg expr) = expressionToBC expr
applicationArgToBC (ApplicationSpreadArg expr) =
  throwError "Spread args not yet supported by compiler"

expressionToBC :: Expression -> ImpCompiler (Vector Instruction)
expressionToBC (EApp application       ) = applicationToBC application
expressionToBC (BinaryOp op expr1 expr2) = do
  expr1Ops <- expressionToBC expr1
  expr2Ops <- expressionToBC expr2
  let instructions = expr1Ops V.++ expr2Ops :: Vector Instruction
  case opToOperator op of
    Just operator -> return $ V.snoc instructions (Operator operator)
    Nothing       -> throwError $ "Unsupported operator: " ++ op
expressionToBC (UnaryOp op expr) = do
  exprOps <- expressionToBC expr
  case opToOperator op of
    Just operator -> return $ V.snoc exprOps (Operator operator)
    Nothing       -> throwError $ "Unsupported operator: " ++ op
expressionToBC (EVar var) = V.singleton <$> variableToBC var
expressionToBC (EVal val) = V.singleton <$> valueToBC val

variableToBC :: Variable -> ImpCompiler Instruction
variableToBC (LocalVariable name) = do
  addr <- getAddress name
  return $ case addr of
    Nothing -> Constant SNull
    Just a  -> Load a
variableToBC (GlobalVariable name) = return $ External name

valueToBC :: Value -> ImpCompiler Instruction
valueToBC (Number n)                = return $ Constant (SFloat n)
valueToBC Null                      = return $ Constant SNull
valueToBC (Symbol             v   ) = return $ Constant (SString v)
valueToBC (BuiltInFunctionRef name) = do
  addr <- getAddress name
  case addr of
    Nothing -> throwError $ "Unknown function: " ++ name
    Just a  -> return $ Load a
