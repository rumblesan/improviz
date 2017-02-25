module Language.Interpreter where

import Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import Language.Interpreter.Types
import Language.Interpreter.Operators

import Language.LanguageAst
import qualified Language.Interpreter.Scope as LS
import qualified Gfx.Ast as GA

emptyState :: InterpreterState
emptyState = InterpreterState {
  variables = LS.empty,
  builtins = M.fromList [],
  blockStack = [],
  currentGfx = GA.emptyGfx,
  gfxStack = []
  }

setVariable :: Identifier -> Value -> InterpreterProcess Value
setVariable name val = modify (\s -> s {
    variables = LS.setVariable (variables s) name val
  }) >> return val

getVariable :: Identifier -> InterpreterProcess Value
getVariable name = do
  s <- get
  case LS.getVariable (variables s) name of
    Just v -> return v
    Nothing -> throwError $ "Could not get variable: " ++ name

getVariableWithDefault :: Identifier -> Value -> InterpreterProcess Value
getVariableWithDefault name defValue = do
  s <- get
  return $ fromMaybe defValue (LS.getVariable (variables s) name)

getVariableWithBackup :: Identifier -> Identifier -> InterpreterProcess Value
getVariableWithBackup name bkp = do
  s <- get
  case LS.getVariable (variables s) name of
    Just v -> return v
    Nothing -> getVariable bkp

setBuiltIn :: Identifier -> BuiltInFunction -> [Identifier] -> InterpreterProcess ()
setBuiltIn name func argNames = modify (\s -> s {
                                  variables = LS.setVariable (variables s) name (BuiltIn argNames),
                                  builtins = M.insert name func (builtins s)
                                  })

getBuiltIn :: Identifier -> InterpreterProcess BuiltInFunction
getBuiltIn name = do
  s <- get
  case M.lookup name (builtins s) of
    Just b -> return b
    Nothing -> throwError $ "Could not get builtin: " ++ name

addGfxCommand :: GA.GfxCommand -> InterpreterProcess ()
addGfxCommand cmd = modify (\s -> s {currentGfx = GA.addGfx (currentGfx s) cmd})

newGfxScope :: InterpreterProcess ()
newGfxScope = modify (\s -> s {
    currentGfx = GA.emptyGfx,
    gfxStack = currentGfx s : gfxStack s
  })

newScope :: InterpreterProcess Value -> InterpreterProcess Value
newScope childScope = do
  modify (\s -> s { variables = LS.newScope (variables s) })
  v <- childScope
  modify (\s -> s { variables = popScope (variables s) })
  return v
 where
   popScope :: LS.ScopeStack k v -> LS.ScopeStack k v
   popScope oldS = case LS.popScope oldS of
     Left _ -> oldS
     Right popped -> popped

pushBlock :: Block -> InterpreterProcess ()
pushBlock b = modify (\s -> s { blockStack = b : blockStack s })

removeBlock :: InterpreterProcess ()
removeBlock = modify (\s -> s { blockStack = tail $ blockStack s })


interpretLanguage :: Block -> InterpreterProcess Value
interpretLanguage = interpretBlock

interpretBlock :: Block -> InterpreterProcess Value
interpretBlock (Block elements) = last <$> mapM interpretElement elements

interpretElement :: Element -> InterpreterProcess Value
interpretElement (ElLoop loop) = interpretLoop loop
interpretElement (ElAssign assignment) = interpretAssignment assignment
interpretElement (ElExpression expression) = interpretExpression expression

interpretApplication :: Application -> InterpreterProcess Value
interpretApplication (Application name args block) = do
  f <- getVariable name
  case f of
    (Lambda argNames lBlock) ->
      newScope (
        do
          tell ["Running lambda"]
          argValues <- mapM interpretExpression args
          zipWithM_ setVariable argNames argValues
          maybe (return ()) pushBlock block
          ret <- interpretBlock lBlock
          when (isJust block) removeBlock
          return ret
      )
    (BuiltIn argNames) ->
      newScope (
        do
          tell ["Running BuiltIn: " ++ name]
          argValues <- mapM interpretExpression args
          zipWithM_ setVariable argNames argValues
          b <- getBuiltIn name
          b block
      )
    _ -> return Null

interpretLoop :: Loop -> InterpreterProcess Value
interpretLoop (Loop num loopVar block) =
  let
    loopNums = fromIntegral <$> [0..(num-1)]
  in
    foldM_ looping Null loopNums >> return Null
  where
    looping _ n = do
      _ <- maybe (return Null) (\vn -> setVariable vn (Number n)) loopVar
      interpretBlock block

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
