module LCLangLite.Interpreter where

import Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import LCLangLite.Interpreter.Types

import LCLangLite.LanguageAst
import qualified LCLangLite.Interpreter.Scope as LS
import qualified Gfx.GfxAst as GA

noop :: (Monad m) => BuiltInFunction m
noop _ = return Null

emptyState :: InterpreterState m
emptyState = InterpreterState {
  variables = LS.empty,
  builtins = M.fromList [],
  blockStack = [],
  currentGfx = GA.emptyGfx,
  gfxStack = []
  }

setVariable :: (Monad m) => Identifier -> Value -> InterpreterProcess m Value
setVariable name val = modify (\s -> s {
                                  variables = LS.setVariable (variables s) name (return val)
                                  }) >> return val

getVariable :: (Monad m) => Identifier -> InterpreterProcess m Value
getVariable name = do
  s <- get
  fromMaybe (return Null) $ LS.getVariable (variables s) name

setBuiltIn :: (Monad m) => Identifier -> BuiltInFunction m -> [Identifier] -> InterpreterProcess m ()
setBuiltIn name func argNames = modify (\s -> s {
                                  variables = LS.setVariable (variables s) name (return $ BuiltIn argNames),
                                  builtins = M.insert name func (builtins s)
                                  })

getBuiltIn :: (Monad m) => Identifier -> InterpreterProcess m (BuiltInFunction m)
getBuiltIn name = do
  s <- get
  return $ fromMaybe noop $ M.lookup name $ builtins s

addGfxCommand :: (Monad m) => GA.GfxCommand -> InterpreterProcess m ()
addGfxCommand cmd = do
  modify (\s -> s {
    currentGfx = GA.addGfx (currentGfx s) cmd
  })

newGfxScope :: (Monad m) => InterpreterProcess m ()
newGfxScope = do
  modify (\s -> s {
    currentGfx = GA.emptyGfx,
    gfxStack = currentGfx s : gfxStack s
  })

newScope :: (Monad m) => InterpreterProcess m Value -> InterpreterProcess m Value
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

pushBlock :: (Monad m) => Block -> InterpreterProcess m ()
pushBlock b = modify (\s -> s { blockStack = b : blockStack s })

removeBlock :: (Monad m) => InterpreterProcess m ()
removeBlock = modify (\s -> s { blockStack = tail $ blockStack s })


interpretLanguage :: (Monad m) => Block -> InterpreterProcess m Value
interpretLanguage = interpretBlock

interpretBlock :: (Monad m) => Block -> InterpreterProcess m Value
interpretBlock (Block elements) = last <$> mapM interpretElement elements

interpretElement :: (Monad m) => Element -> InterpreterProcess m Value
interpretElement (ElLoop loop) = interpretLoop loop
interpretElement (ElAssign assignment) = interpretAssignment assignment
interpretElement (ElExpression expression) = do
  s1 <- get
  val <- interpretExpression expression
  s2 <- get
  return val

interpretApplication :: (Monad m) => Application -> InterpreterProcess m Value
interpretApplication (Application name args block) = do
  f <- getVariable name
  v <- case f of
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
          res <- b block
          s <- get
          return res
      )
    _ -> return Null
  s <- get
  return v

interpretLoop :: (Monad m) => Loop -> InterpreterProcess m Value
interpretLoop (Loop num loopVar block) =
  let
    loopNums = fromIntegral <$> [0..(num-1)]
  in
    foldM_ looping Null loopNums >> return Null
  where
    looping _ n = do
      _ <- maybe (return Null) (\vn -> setVariable vn (Number n)) loopVar
      interpretBlock block

interpretAssignment :: (Monad m) => Assignment -> InterpreterProcess m Value
interpretAssignment (Assignment name expression) = do
  value <- interpretExpression expression
  setVariable name value

interpretExpression :: (Monad m) => Expression -> InterpreterProcess m Value
interpretExpression (EApp application) = do
  v <- interpretApplication application
  s <- get
  return v
interpretExpression (BinaryOp _ _ _) = undefined
interpretExpression (UnaryOp _ _) = undefined
interpretExpression (EVar (Variable varName)) = getVariable varName
interpretExpression (EVal value) = return value
