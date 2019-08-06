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

setFunction :: Identifier -> Func -> InterpreterProcess ()
setFunction name (Func fname args body) = do
  f <- use functions
  assign functions (M.insert name (UserFunctionDef fname args body) f)

getFunction :: Identifier -> InterpreterProcess UserFunctionDef
getFunction name = do
  functionDefs <- use functions
  case M.lookup name functionDefs of
    Just f  -> return f
    Nothing -> throwError $ "Could not find function: " ++ name

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

interpretApplication :: Application -> InterpreterProcess Value
interpretApplication f@(Application name args mbBlock) = do
  v <- interpretVariable name
  case v of
    (BlockRef        blk ) -> interpretBlock blk
    (UserFunctionRef name) -> newScope $ do
      (UserFunctionDef _ argNames body) <- getFunction name
      assignApplicationArgs argNames args mbBlock
      interpretBlock body
    (BuiltInFunctionRef name) -> newScope $ do
      (BuiltInFunction action) <- getBuiltIn name
      argValues                <- mapM interpretExpression args
      action argValues
    _ -> return Null

assignApplicationArgs
  :: [FuncArg] -> [Expression] -> Maybe Block -> InterpreterProcess Value
assignApplicationArgs funcArgs args mbBlock = do
  argValues <- mapM interpretExpression args
  zipWithM_ (assignArg mbBlock) funcArgs (argValues ++ repeat Null)
  return Null
 where
  assignArg _  (VarArg   name) v = setVariable name v
  assignArg mb (BlockArg name) _ = setVariable name $ maybe Null BlockRef mb

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
interpretFunc f@(Func name argNames lBlock) = do
  setFunction name f
  setGlobal name (UserFunctionRef name)

interpretIf :: If -> InterpreterProcess Value
interpretIf (If pred block1 block2) = do
  p <- isZero pred
  if p then interpretBlock block1 else maybe (return Null) interpretBlock block2
 where
  isZero :: Expression -> InterpreterProcess Bool
  isZero e = do
    v <- interpretExpression e
    return $ case v of
      Number 0 -> False
      _        -> True

loopNums :: Expression -> InterpreterProcess [Float]
loopNums numExpr = do
  loopV <- interpretExpression numExpr
  case loopV of
    Number v          -> return [0 .. (v - 1)]
    Null              -> throwError "Null given as loop number expression"
    Symbol          _ -> throwError "Symbol given as loop number expression"
    BlockRef        _ -> throwError "Block given as loop number expression"
    UserFunctionRef _ -> throwError "Function given as loop number expression"
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
interpretExpression (EApp application ) = interpretApplication application
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
