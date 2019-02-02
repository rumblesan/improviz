module Language.Interpreter
  ( emptyState
  , getEngineInfo
  , setBuiltIn
  , getGlobalNames
  , setVariable
  , getVariable
  , getVariableWithDefault
  , getVarOrNull
  , getNumberFromNull
  , addGfxCommand
  , gfxScopedBlock
  , setGfxBackground
  , setAnimationStyle
  , interpretLanguage
  , interpretBlock
  , interpretExpression
  )
where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as S

import           Language.Interpreter.Operators
import           Language.Interpreter.Types

import qualified Gfx.Ast                       as GA
import qualified Gfx.EngineState               as GE
import           Gfx.PostProcessing             ( AnimationStyle(..) )
import           Gfx.Types                      ( Colour(..) )
import           Language.Ast
import qualified Language.Interpreter.Scope    as LS

emptyState :: InterpreterState
emptyState = InterpreterState { variables      = LS.empty
                              , globals        = M.fromList []
                              , builtins       = M.fromList []
                              , functions      = M.fromList []
                              , gfxBackground  = Colour 1 1 1 1
                              , currentGfx     = GA.emptyGfx
                              , animationStyle = NormalStyle
                              , gfxStack       = []
                              , engineInfo     = GE.EngineInfo M.empty
                              }

getEngineInfo :: InterpreterProcess GE.EngineInfo
getEngineInfo = gets engineInfo

setFunction :: Identifier -> Func -> InterpreterProcess ()
setFunction name (Func fname args body) = modify
  (\s -> s
    { functions = M.insert name (UserFunctionDef fname args body) (functions s)
    }
  )

getFunction :: Identifier -> InterpreterProcess UserFunctionDef
getFunction name = do
  functionDefs <- gets functions
  case M.lookup name functionDefs of
    Just f  -> return f
    Nothing -> throwError $ "Could not find function: " ++ name

setBuiltIn
  :: Identifier
  -> InterpreterProcess Value
  -> [FuncArg]
  -> InterpreterProcess ()
setBuiltIn name func funcArgs = modify
  (\s -> s
    { globals  = M.insert name (BuiltInFunctionRef name) (globals s)
    , builtins = M.insert name (BuiltInFunction funcArgs func) (builtins s)
    }
  )

getBuiltIn :: Identifier -> InterpreterProcess BuiltInFunction
getBuiltIn name = do
  builtInDefs <- gets builtins
  case M.lookup name builtInDefs of
    Just b  -> return b
    Nothing -> throwError $ "Could not get builtin: " ++ name

setGlobal :: Identifier -> Value -> InterpreterProcess Value
setGlobal name val =
  modify (\s -> s { globals = M.insert name val (globals s) }) >> return val

getGlobal :: Identifier -> InterpreterProcess Value
getGlobal name = do
  globalDefs <- gets globals
  case M.lookup name globalDefs of
    Just v  -> return v
    Nothing -> throwError $ "Could not get global: " ++ name

getGlobalNames :: InterpreterProcess (S.Set String)
getGlobalNames = gets (M.keysSet . globals)

setVariable :: Identifier -> Value -> InterpreterProcess Value
setVariable name val =
  modify (\s -> s { variables = LS.setVariable (variables s) name val })
    >> return val

getVariable :: Identifier -> InterpreterProcess Value
getVariable name = do
  variableDefs <- gets variables
  case LS.getVariable variableDefs name of
    Just v  -> return v
    Nothing -> throwError $ "Could not get variable: " ++ name

getVariableWithDefault :: Identifier -> Value -> InterpreterProcess Value
getVariableWithDefault name defValue = do
  variableDefs <- gets variables
  return $ case fromMaybe Null (LS.getVariable variableDefs name) of
    Null -> defValue
    v    -> v

getVarOrNull :: Identifier -> InterpreterProcess Value
getVarOrNull name = do
  variableDefs <- gets variables
  return $ fromMaybe Null (LS.getVariable variableDefs name)

getNumberFromNull :: Value -> Float -> Float
getNumberFromNull Null         def = def
getNumberFromNull (Number val) _   = val
getNumberFromNull _            def = def

addGfxCommand :: GA.GfxCommand -> InterpreterProcess ()
addGfxCommand cmd =
  modify (\s -> s { currentGfx = GA.addGfx (currentGfx s) cmd })

gfxScopedBlock :: GA.GfxCommand -> Block -> InterpreterProcess ()
gfxScopedBlock scopedCommand block = do
  addGfxCommand $ GA.ScopeCommand GA.PushScope
  addGfxCommand scopedCommand
  interpretBlock block
  addGfxCommand $ GA.ScopeCommand GA.PopScope

newScope :: InterpreterProcess Value -> InterpreterProcess Value
newScope childScope = do
  modify (\s -> s { variables = LS.newScope (variables s) })
  v <- childScope
  modify (\s -> s { variables = popScope (variables s) })
  return v
 where
  popScope :: LS.ScopeStack k v -> LS.ScopeStack k v
  popScope oldS = case LS.popScope oldS of
    Left  _      -> oldS
    Right popped -> popped

setGfxBackground :: (Float, Float, Float) -> InterpreterProcess Value
setGfxBackground (r, g, b) =
  modify (\s -> s { gfxBackground = Colour r g b 1 }) >> return Null

setAnimationStyle :: AnimationStyle -> InterpreterProcess Value
setAnimationStyle style =
  modify (\s -> s { animationStyle = style }) >> return Null

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
interpretApplication f@(Application name args block) = do
  v <- interpretVariable name
  case v of
    (BlockRef blk) -> do
      interpretBlock blk
    (UserFunctionRef name) -> do
      userFunc <- getFunction name
      interpretUserFunctionApplication f userFunc
    (BuiltInFunctionRef name) -> do
      builtInFunc <- getBuiltIn name
      interpretBuiltInFunctionApplication f builtInFunc
    _ -> return Null

interpretUserFunctionApplication
  :: Application -> UserFunctionDef -> InterpreterProcess Value
interpretUserFunctionApplication (Application _ args mbBlock) (UserFunctionDef _ funcArgs body)
  = newScope
    (do
      assignApplicationArgs funcArgs args mbBlock
      ret <- interpretBlock body
      return ret
    )

interpretBuiltInFunctionApplication
  :: Application -> BuiltInFunction -> InterpreterProcess Value
interpretBuiltInFunctionApplication (Application _ args mbBlock) (BuiltInFunction funcArgs action)
  = newScope
    (do
      assignApplicationArgs funcArgs args mbBlock
      ret <- action
      return ret
    )

assignApplicationArgs
  :: [FuncArg] -> [Expression] -> Maybe Block -> InterpreterProcess Value
assignApplicationArgs funcArgs args mbBlock = do
  argValues <- mapM interpretExpression args
  zipWithM_ (assignArg mbBlock) funcArgs (argValues ++ repeat Null)
  return Null
 where
  assignArg _ (VarArg name) v = setVariable name v
  assignArg mb (BlockArg name) _ =
    setVariable name $ maybe Null (\b -> BlockRef b) mb

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
  var <- getVarOrNull name
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
interpretExpression (EVar var  ) = interpretVariable var
interpretExpression (EVal value) = return value

interpretVariable :: Variable -> InterpreterProcess Value
interpretVariable (LocalVariable  varName) = getVariable varName
interpretVariable (GlobalVariable varName) = getGlobal varName
