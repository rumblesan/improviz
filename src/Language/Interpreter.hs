module Language.Interpreter
  ( interpretLanguage
  )
where

import           Control.Monad                  ( zipWithM_
                                                , foldM
                                                , foldM_
                                                )
import           Control.Monad.Except           ( throwError )
import           Lens.Simple                    ( use
                                                , assign
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Safe                           ( atMay )

import           Language.Interpreter.Operators
import           Language.Interpreter.Types
import           Language.Interpreter.Values    ( getValueType )

import           Language.Ast
import qualified Language.Interpreter.Scope    as LS

interpretLanguage :: Program -> InterpreterProcess Value
interpretLanguage (Program statements) =
  last <$> mapM interpretStatement statements

interpretStatement :: Statement -> InterpreterProcess Value
interpretStatement (StLoop       loop      ) = interpretLoop loop
interpretStatement (StAssign     assignment) = interpretAssignment assignment
interpretStatement (StIf         ifElem    ) = interpretIf ifElem
interpretStatement (StFunc       funcElem  ) = interpretFunc funcElem
interpretStatement (StExpression expression) = interpretExpression expression

interpretBlock :: Block -> InterpreterProcess Value
interpretBlock (Block elements) = last <$> mapM interpretElement elements

interpretElement :: Element -> InterpreterProcess Value
interpretElement (ElLoop       loop      ) = interpretLoop loop
interpretElement (ElAssign     assignment) = interpretAssignment assignment
interpretElement (ElIf         ifElem    ) = interpretIf ifElem
interpretElement (ElExpression expression) = interpretExpression expression

interpretApplication
  :: Variable -> [ApplicationArg] -> Maybe Lambda -> InterpreterProcess Value
interpretApplication name args mbLmb = do
  v         <- interpretVariable name
  argValues <- foldM handleArg [] args
  case v of
    (BuiltInFunctionRef name) -> newScope $ do
      (BuiltInFunction action) <- getBuiltIn name
      action argValues
    (LambdaRef lmb) -> interpretFunctionCall lmb argValues mbLmb
    ot ->
      throwError
        $  show name
        ++ " cannot be applied as it is of type "
        ++ getValueType ot

interpretFunctionCall
  :: Lambda -> [Value] -> Maybe Lambda -> InterpreterProcess Value
interpretFunctionCall (Lambda argNames (Just closureScope) body) argValues mbLmb
  = do
    existingScope <- use variables
    assign variables closureScope
    assignApplicationArgs argNames argValues mbLmb
    v <- interpretBlock body
    assign variables existingScope
    return v
interpretFunctionCall (Lambda argNames Nothing body) argValues mbLmb = do
  assignApplicationArgs argNames argValues mbLmb
  interpretBlock body

handleArg :: [Value] -> ApplicationArg -> InterpreterProcess [Value]
handleArg vals (ApplicationSingleArg expr) =
  (\v -> vals ++ [v]) <$> interpretExpression expr
handleArg vals (ApplicationSpreadArg expr) = do
  val <- interpretExpression expr
  case val of
    (VList l) -> return (vals ++ l)
    _         -> throwError "Must be given a list to use as a spread argument"

assignApplicationArgs
  :: [FuncArg] -> [Value] -> Maybe Lambda -> InterpreterProcess Value
assignApplicationArgs funcArgs argValues mbLmb = do
  zipWithM_ (assignArg mbLmb) funcArgs (argValues ++ repeat Null)
  setVariable "args" $ VList argValues
  return Null
 where
  assignArg _ (VarArg name defaultValue) v = case v of
    Null -> setVariable name defaultValue
    _    -> setVariable name v
  assignArg ml (BlockArg name) _ = setVariable name $ maybe Null LambdaRef ml

interpretLoop :: Loop -> InterpreterProcess Value
interpretLoop (Loop numExpr loopVar block) = do
  nums <- loopNums numExpr
  foldM_ looping Null nums >> return Null
 where
  looping :: Value -> Float -> InterpreterProcess Value
  looping _ n = do
    _ <- maybe (return Null) (\vn -> setVariable vn (Number n)) loopVar
    interpretBlock block

interpretFunc :: Func -> InterpreterProcess Value
interpretFunc f@(Func name args lBlock) = do
  scope <- use variables
  setVariable name (LambdaRef $ Lambda args (Just scope) lBlock)

interpretIf :: If -> InterpreterProcess Value
interpretIf (If predBlocks) = loopBlocks predBlocks
 where
  isTruthy :: Expression -> InterpreterProcess Bool
  isTruthy e = (\v -> v /= Number 0) <$> interpretExpression e
  loopBlocks :: [(Expression, Block)] -> InterpreterProcess Value
  loopBlocks ((pred, blk) : rest) = do
    p <- isTruthy pred
    if p then interpretBlock blk else loopBlocks rest
  loopBlocks [] = return Null

loopNums :: Expression -> InterpreterProcess [Float]
loopNums numExpr = do
  loopV <- interpretExpression numExpr
  case loopV of
    Number v -> return [0 .. (v - 1)]
    Null     -> throwError "Null given as loop number expression"
    Symbol _ -> throwError "Symbol given as loop number expression"
    BuiltInFunctionRef _ ->
      throwError "Function given as loop number expression"

interpretAssignment :: Assignment -> InterpreterProcess Value
interpretAssignment (AbsoluteAssignment name expression) =
  interpretExpression expression >>= setVariable name
interpretAssignment (ConditionalAssignment name expression) = do
  variableDefs <- use variables
  let var = fromMaybe Null (LS.getVariable variableDefs name)
  case var of
    Null -> interpretExpression expression >>= setVariable name
    _    -> return var

setLambdaScope :: Maybe (LS.ScopeStack Identifier Value) -> Lambda -> Lambda
setLambdaScope scope (Lambda argNames _ block) = Lambda argNames scope block

interpretExpression :: Expression -> InterpreterProcess Value
interpretExpression (EApp (Application name args mbLambda)) = do
  variableDefs <- use variables
  let mbScopedLambda = fmap (setLambdaScope (Just variableDefs)) mbLambda
  interpretApplication name args mbScopedLambda
interpretExpression (BinaryOp op v1 v2) = do
  n1 <- interpretExpression v1
  n2 <- interpretExpression v2
  binaryOp op n1 n2
interpretExpression (UnaryOp op v) = do
  n <- interpretExpression v
  unaryOp op n
interpretExpression (EVar  var             ) = interpretVariable var
interpretExpression (EVal  value           ) = return value
interpretExpression (EList exprs) = VList <$> mapM interpretExpression exprs
interpretExpression (EAccess lexpr position) = do
  p <- interpretExpression position
  l <- interpretExpression lexpr
  case (l, p) of
    (VList list, Number v) -> maybe
      (throwError "Accessor index out of range")
      return
      (atMay list $ floor v)
    (VList _, _) -> throwError "Need to give number expression in accessor"
    (_, Number _) -> throwError "Need to access list expression"

interpretVariable :: Variable -> InterpreterProcess Value
interpretVariable (LocalVariable  varName) = getVariable varName
interpretVariable (GlobalVariable varName) = getGlobal varName
