module Language.Interpreter where

import           System.Random

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
import           Gfx.Types                      (Colour (..))
import           Language.Ast
import qualified Language.Interpreter.Scope     as LS

emptyState :: InterpreterState
emptyState =
  InterpreterState
    { variables = LS.empty
    , builtins = M.fromList []
    , blockStack = []
    , gfxBackground = Colour 1 1 1 1
    , currentGfx = GA.emptyGfx
    , animationStyle = NormalStyle
    , gfxStack = []
    , engineInfo = GE.EngineInfo M.empty
    , rng = mkStdGen 0
    }

getEngineInfo :: InterpreterProcess GE.EngineInfo
getEngineInfo = gets engineInfo

seedRNG :: Int -> InterpreterProcess Value
seedRNG seed = modify (\s -> s {rng = mkStdGen seed}) >> return Null

getRNG :: InterpreterProcess StdGen
getRNG = gets rng

setRNG :: StdGen -> InterpreterProcess Value
setRNG newRNG = modify (\s -> s {rng = newRNG}) >> return Null

getRandom :: InterpreterProcess Value
getRandom = do
  (v, nextGen) <- random <$> gets rng
  modify (\s -> s {rng = nextGen})
  return $ Number v

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
         { variables = LS.setVariable (variables s) name (BuiltIn name argNames)
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
  modify (\s -> s {gfxBackground = Colour r g b 1}) >> return Null

setAnimationStyle :: AnimationStyle -> InterpreterProcess Value
setAnimationStyle style =
  modify (\s -> s {animationStyle = style}) >> return Null

-- Interpreter Logic
interpretLanguage :: Program -> InterpreterProcess Value
interpretLanguage (Program statements) =
  last <$> mapM interpretStatement statements

interpretStatement :: Statement -> InterpreterProcess Value
interpretStatement (StLoop loop)             = interpretLoop loop
interpretStatement (StAssign assignment)     = interpretAssignment assignment
interpretStatement (StIf ifElem)             = interpretIf ifElem
interpretStatement (StFunc funcElem)         = interpretFunc funcElem
interpretStatement (StExpression expression) = interpretExpression expression

interpretBlock :: Block -> InterpreterProcess Value
interpretBlock (Block elements) = last <$> mapM interpretElement elements

interpretElement :: Element -> InterpreterProcess Value
interpretElement (ElLoop loop)             = interpretLoop loop
interpretElement (ElAssign assignment)     = interpretAssignment assignment
interpretElement (ElIf ifElem)             = interpretIf ifElem
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
    (BuiltIn funcName argNames) ->
      newScope
        (do tell ["Running BuiltIn: " ++ funcName]
            _ <- handleBuiltInArgs argNames args
            b <- getBuiltIn funcName
            pushBlock block
            ret <- b
            popBlock
            return ret)
    _ -> return Null

handleLambdaArgs :: [Identifier] -> [Expression] -> InterpreterProcess Value
handleLambdaArgs argNames args = do
  argValues <- mapM interpretExpression args
  zipWithM_ defaultArg argNames (argValues ++ repeat Null)
  return Null
  where
    defaultArg name argVal = setVariable name argVal

handleBuiltInArgs :: [Identifier] -> [Expression] -> InterpreterProcess Value
handleBuiltInArgs argNames args = do
  argValues <- mapM interpretExpression args
  zipWithM_ setVariable argNames (argValues ++ repeat Null)
  return Null

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
    Number v    -> return [0 .. (v - 1)]
    Null        -> throwError "Null given as loop number expression"
    Lambda _ _  -> throwError "Function given as loop number expression"
    BuiltIn _ _ -> throwError "Function given as loop number expression"

interpretAssignment :: Assignment -> InterpreterProcess Value
interpretAssignment (AbsoluteAssignment name expression) =
  interpretExpression expression >>= setVariable name
interpretAssignment (ConditionalAssignment name expression) = do
  var <- getVarOrNull name
  case var of
    Null -> interpretExpression expression >>= setVariable name
    _    -> return var

interpretExpression :: Expression -> InterpreterProcess Value
interpretExpression (EApp application) = interpretApplication application
interpretExpression (BinaryOp op v1 v2) = do
  n1 <- interpretExpression v1
  n2 <- interpretExpression v2
  binaryOp op n1 n2
interpretExpression (UnaryOp op v) = do
  n <- interpretExpression v
  unaryOp op n
interpretExpression (EVar var) = interpretVariable var
interpretExpression (EVal value) = return value

interpretVariable :: Variable -> InterpreterProcess Value
interpretVariable (LocalVariable varName)  = getVariable varName
interpretVariable (GlobalVariable varName) = getVariable varName
