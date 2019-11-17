module Language.Interpreter
  ( emptyState
  , getTextureInfo
  , setBuiltIn
  , getGlobalNames
  , setVariable
  , setGlobal
  , getVariable
  , getExternal
  , interpretLanguage
  , useGfxCtx
  )
where

import           Control.Monad                  ( zipWithM_
                                                , foldM
                                                , foldM_
                                                )
import           Control.Monad.Trans            ( liftIO )
import           Control.Monad.Except           ( throwError )
import           System.Random                  ( mkStdGen )
import           Lens.Simple                    ( use
                                                , assign
                                                )
import           Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as S
import           Safe                           ( atMay )

import           Language.Interpreter.Operators
import           Language.Interpreter.Types
import           Language.Interpreter.Values    ( getValueType )

import           Gfx.Textures                   ( TextureInfo(..) )
import           Gfx.Context                    ( GfxContext
                                                , emptyGfxContext
                                                )
import           Language.Ast
import qualified Language.Interpreter.Scope    as LS

emptyState :: InterpreterState
emptyState = InterpreterState { _variables   = LS.empty
                              , _externals   = M.empty
                              , _globals     = M.empty
                              , _builtins    = M.empty
                              , _functions   = M.empty
                              , _textureInfo = TextureInfo M.empty
                              , _gfxContext  = emptyGfxContext
                              , _rnGen       = mkStdGen 1234
                              }

getTextureInfo :: String -> InterpreterProcess (Maybe Int)
getTextureInfo name = do
  ti <- use textureInfo
  return $ M.lookup name $ textureFrames ti

setBuiltIn
  :: Identifier
  -> ([Value] -> InterpreterProcess Value)
  -> InterpreterProcess ()
setBuiltIn name func = do
  g <- use globals
  assign globals (M.insert name (BuiltInFunctionRef name) g)
  b <- use builtins
  assign builtins (M.insert name (BuiltInFunction func) b)

getBuiltIn :: Identifier -> InterpreterProcess BuiltInFunction
getBuiltIn name = do
  builtInDefs <- use builtins
  case M.lookup name builtInDefs of
    Just b  -> return b
    Nothing -> throwError $ "Could not get builtin: " ++ name

setGlobal :: Identifier -> Value -> InterpreterProcess Value
setGlobal name val = do
  g <- use globals
  assign globals (M.insert name val g)
  return val

getGlobal :: Identifier -> InterpreterProcess Value
getGlobal name = do
  globalDefs <- use globals
  case M.lookup name globalDefs of
    Just v  -> return v
    Nothing -> throwError $ "Could not get global: " ++ name

getGlobalNames :: InterpreterProcess (S.Set String)
getGlobalNames = M.keysSet <$> use globals

setVariable :: Identifier -> Value -> InterpreterProcess Value
setVariable name val = do
  v <- use variables
  assign variables (LS.setVariable v name val)
  return val

getVariable :: Identifier -> InterpreterProcess Value
getVariable name = do
  variableDefs <- use variables
  case LS.getVariable variableDefs name of
    Just v  -> return v
    Nothing -> throwError $ "Could not get variable: " ++ name

getExternal :: Identifier -> Value -> InterpreterProcess Value
getExternal name defaultValue = do
  externalVars <- use externals
  return $ fromMaybe defaultValue (M.lookup name externalVars)

useGfxCtx :: (GfxContext -> IO ()) -> InterpreterProcess ()
useGfxCtx action = do
  ctx <- use gfxContext
  liftIO $ action ctx

newScope :: InterpreterProcess Value -> InterpreterProcess Value
newScope childScope = do
  varB <- use variables
  assign variables (LS.newScope varB)
  v    <- childScope
  varA <- use variables
  assign variables (popScope varA)
  return v
 where
  popScope :: LS.ScopeStack k v -> LS.ScopeStack k v
  popScope oldS = case LS.popScope oldS of
    Left  _      -> oldS
    Right popped -> popped

-- Interpreter Logic
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
  return Null
 where
  assignArg _  (VarArg   name) v = setVariable name v
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

interpretExpression :: Expression -> InterpreterProcess Value
interpretExpression (EApp (Application name args mbBlock)) = do
  variableDefs <- use variables
  let mbLmb = fmap (Lambda [] (Just variableDefs)) mbBlock
  interpretApplication name args mbLmb
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
