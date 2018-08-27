module Language.Interpreter where

import           Graphics.Rendering.OpenGL      (Color4 (..))

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import qualified Data.List                      as L
import           Data.Map.Strict                as M
import           Data.Maybe                     (fromMaybe, isJust)

import           Language.Interpreter.Operators
import           Language.Interpreter.Types

import qualified Gfx.Ast                        as GA
import qualified Gfx.EngineState                as GE
import           Gfx.PostProcessing             (AnimationStyle (..))
import           Language.Ast
import qualified Language.Interpreter.Scope     as LS

emptyState :: InterpreterState
emptyState =
  InterpreterState
    { variables = LS.empty
    , builtins = M.fromList []
    , blockStack = []
    , gfxBackground = Color4 1 1 1 1
    , currentGfx = GA.emptyGfx
    , animationStyle = NormalStyle
    , gfxStack = []
    , engineInfo = GE.EngineInfo M.empty
    }

getEngineInfo :: InterpreterProcess GE.EngineInfo
getEngineInfo = gets engineInfo

setVariable :: Identifier -> Value -> InterpreterProcess Value
setVariable name val =
  modify (\s -> s {variables = LS.setVariable (variables s) name val}) >>
  return val

getVariable :: Identifier -> InterpreterProcess Value
getVariable name = do
  s <- get
  case LS.getVariable (variables s) name of
    Just v  -> return v
    Nothing -> throwError $ "Could not get variable: " ++ name

getVariableWithDefault :: Identifier -> Value -> InterpreterProcess Value
getVariableWithDefault name defValue = do
  s <- get
  return $
    case fromMaybe Null (LS.getVariable (variables s) name) of
      Null -> defValue
      v    -> v

getVarOrNull :: Identifier -> InterpreterProcess Value
getVarOrNull name = do
  s <- get
  return $ fromMaybe Null (LS.getVariable (variables s) name)

getNumberFromNull :: Value -> Float -> Float
getNumberFromNull Null def       = def
getNumberFromNull (Number val) _ = val
getNumberFromNull _ def          = def

setBuiltIn ::
     Identifier -> BuiltInFunction -> [Identifier] -> InterpreterProcess ()
setBuiltIn name func argNames =
  modify
    (\s ->
       s
         { variables = LS.setVariable (variables s) name (BuiltIn argNames)
         , builtins = M.insert name func (builtins s)
         })

getBuiltIn :: Identifier -> InterpreterProcess BuiltInFunction
getBuiltIn name = do
  s <- get
  case M.lookup name (builtins s) of
    Just b  -> return b
    Nothing -> throwError $ "Could not get builtin: " ++ name

addGfxCommand :: GA.GfxCommand -> InterpreterProcess ()
addGfxCommand cmd = modify (\s -> s {currentGfx = GA.addGfx (currentGfx s) cmd})

newGfxScope :: InterpreterProcess ()
newGfxScope =
  modify
    (\s -> s {currentGfx = GA.emptyGfx, gfxStack = currentGfx s : gfxStack s})

newScope :: InterpreterProcess Value -> InterpreterProcess Value
newScope childScope = do
  modify (\s -> s {variables = LS.newScope (variables s)})
  v <- childScope
  modify (\s -> s {variables = popScope (variables s)})
  return v
  where
    popScope :: LS.ScopeStack k v -> LS.ScopeStack k v
    popScope oldS =
      case LS.popScope oldS of
        Left _       -> oldS
        Right popped -> popped

pushBlock :: Maybe Block -> InterpreterProcess ()
pushBlock b = modify (\s -> s {blockStack = b : blockStack s})

getBlock :: InterpreterProcess (Maybe Block)
getBlock = gets (head . blockStack)

popBlock :: InterpreterProcess (Maybe Block)
popBlock = do
  b <- getBlock
  modify (\s -> s {blockStack = tail $ blockStack s})
  return b

setGfxBackground :: (Float, Float, Float) -> InterpreterProcess Value
setGfxBackground (r, g, b) =
  modify (\s -> s {gfxBackground = Color4 r g b 1}) >> return Null

setAnimationStyle :: AnimationStyle -> InterpreterProcess Value
setAnimationStyle style =
  modify (\s -> s {animationStyle = style}) >> return Null

interpretLanguage :: Block -> InterpreterProcess Value
interpretLanguage = interpretBlock

interpretBlock :: Block -> InterpreterProcess Value
interpretBlock (Block elements) = last <$> mapM interpretElement elements

interpretElement :: Element -> InterpreterProcess Value
interpretElement (ElLoop loop)             = interpretLoop loop
interpretElement (ElAssign assignment)     = interpretAssignment assignment
interpretElement (ElIf ifElem)             = interpretIf ifElem
interpretElement (ElFunc funcElem)         = interpretFunc funcElem
interpretElement (ElExpression expression) = interpretExpression expression

interpretApplication :: Application -> InterpreterProcess Value
interpretApplication (Application name args block) = do
  f <- getVariable name
  case f of
    (Lambda argNames lBlock) ->
      newScope
        (do tell ["Running lambda"]
            _ <- handleLambdaArgs argNames args
            pushBlock block
            ret <- interpretBlock lBlock
            popBlock
            return ret)
    (BuiltIn argNames) ->
      newScope
        (do tell ["Running BuiltIn: " ++ name]
            _ <- handleBuiltInArgs argNames args
            b <- getBuiltIn name
            pushBlock block
            ret <- b
            popBlock
            return ret)
    _ -> return Null

handleLambdaArgs ::
     [FunctionArg] -> [ApplicationArg] -> InterpreterProcess Value
handleLambdaArgs funcArgs aargs =
  let (named, non) = L.partition namedArg aargs
      argNames = (\(FunctionArg name _) -> name) <$> funcArgs
   in do nonNamedArgValues <- mapM interpretExpression (getArgExpr <$> non)
         zipWithM_ defaultArg funcArgs (nonNamedArgValues ++ repeat Null)
         mapM_ (assignNamedArg argNames) named
         return Null
  where
    namedArg (ApplicationArg name _) = isJust name
    getArgExpr (ApplicationArg _ expr) = expr
    defaultArg (FunctionArg name mayVal) argVal =
      setVariable name $ fromMaybe argVal mayVal

handleBuiltInArgs ::
     [Identifier] -> [ApplicationArg] -> InterpreterProcess Value
handleBuiltInArgs argNames aargs =
  let (named, non) = L.partition namedArg aargs
   in do nonNamedArgValues <- mapM interpretExpression (getArgExpr <$> non)
         zipWithM_ setVariable argNames (nonNamedArgValues ++ repeat Null)
         mapM_ (assignNamedArg argNames) named
         return Null
  where
    namedArg (ApplicationArg name _) = isJust name
    getArgExpr (ApplicationArg _ expr) = expr

assignNamedArg :: [Identifier] -> ApplicationArg -> InterpreterProcess Value
assignNamedArg argnames (ApplicationArg Nothing expr) = return Null
assignNamedArg argnames (ApplicationArg (Just name) expr) =
  if name `L.elem` argnames
    then interpretExpression expr >>= setVariable name
    else return Null

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
interpretFunc (Func name argNames lBlock) =
  setVariable name (Lambda argNames lBlock)

interpretIf :: If -> InterpreterProcess Value
interpretIf (If pred block1 block2) = do
  p <- isZero pred
  if p
    then interpretBlock block1
    else maybe (return Null) interpretBlock block2
  where
    isZero :: Expression -> InterpreterProcess Bool
    isZero e = do
      v <- interpretExpression e
      return $
        case v of
          Number 0 -> False
          _        -> True

loopNums :: Expression -> InterpreterProcess [Float]
loopNums numExpr = do
  loopV <- interpretExpression numExpr
  case loopV of
    Number v   -> return [0 .. (v - 1)]
    Null       -> throwError "Null given as loop number expression"
    Lambda _ _ -> throwError "Function given as loop number expression"
    BuiltIn _  -> throwError "Function given as loop number expression"

interpretAssignment :: Assignment -> InterpreterProcess Value
interpretAssignment (Assignment name expression) =
  interpretExpression expression >>= setVariable name

interpretExpression :: Expression -> InterpreterProcess Value
interpretExpression (EApp application) = interpretApplication application
interpretExpression (BinaryOp op v1 v2) = do
  n1 <- interpretExpression v1
  n2 <- interpretExpression v2
  binaryOp op n1 n2
interpretExpression (UnaryOp op v) = do
  n <- interpretExpression v
  unaryOp op n
interpretExpression (EVar (Variable varName)) = getVariable varName
interpretExpression (EVal value) = return value
