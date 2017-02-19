module LCLangLite.LanguageInterpreter where

import Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Control.Monad.State.Strict

import LCLangLite.LanguageAst


data InterpreterState m = InterpreterState {
  variables :: Map Identifier (InterpreterProcess m Value),
  blockStack :: [Block]
}

emptyState :: InterpreterState m
emptyState = InterpreterState { variables = M.fromList [], blockStack = [] }

setVariable :: (Monad m) => Identifier -> Value -> InterpreterProcess m Value
setVariable name val = modify (\s -> s { variables = M.insert name (return val) (variables s) }) >> return val

getVariable :: (Monad m) => Identifier -> InterpreterProcess m Value
getVariable name = do
  s <- get
  fromMaybe (return Null) $ M.lookup name $ variables s

newScope :: (Monad m) => InterpreterProcess m Value -> InterpreterProcess m Value
newScope childScope = do
  s <- get
  v <- childScope
  put s
  return v

pushBlock :: (Monad m) => Block -> InterpreterProcess m ()
pushBlock b = modify (\s -> s { blockStack = b : blockStack s })

removeBlock :: (Monad m) => InterpreterProcess m ()
removeBlock = modify (\s -> s { blockStack = tail $ blockStack s })

type InterpreterProcess m v = StateT (InterpreterState m) m v

interpretLanguage :: (Monad m) => Block -> InterpreterProcess m Value
interpretLanguage = interpretBlock

interpretBlock :: (Monad m) => Block -> InterpreterProcess m Value
interpretBlock (Block elements) = last <$> mapM interpretElement elements

interpretElement :: (Monad m) => Element -> InterpreterProcess m Value
interpretElement (ElLoop loop) = interpretLoop loop
interpretElement (ElAssign assignment) = interpretAssignment assignment
interpretElement (ElExpression expression) = interpretExpression expression

-- TODO
-- figure out scoping around function arg blocks
interpretApplication :: (Monad m) => Application -> InterpreterProcess m Value
interpretApplication (Application name args block) = do
  f <- getVariable name
  case f of
    (Lambda argNames lBlock) ->
      newScope (
        do
          argValues <- mapM interpretExpression args
          zipWithM_ setVariable argNames argValues
          maybe (return ()) pushBlock block
          ret <- interpretBlock lBlock
          when (isJust block) removeBlock
          return ret
      )
    _ -> return Null

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
interpretExpression (EApp application) = interpretApplication application
interpretExpression (BinaryOp _ _ _) = undefined
interpretExpression (UnaryOp _ _) = undefined
interpretExpression (EVar (Variable varName)) = getVariable varName
interpretExpression (EVal value) = return value
