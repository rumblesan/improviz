module LCLangLite.LanguageInterpreter where

import Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict

import LCLangLite.LanguageAst


data InterpreterState m = InterpreterState {
  variables :: Map Identifier (InterpreterProcess m)
}

emptyState :: InterpreterState m
emptyState = InterpreterState { variables = M.fromList [] }

setVariable :: (Monad m) => Identifier -> Value -> InterpreterProcess m
setVariable name val = modify updateVars >> return val
  where
    updateVars s = s { variables = M.insert name (return val) (variables s) }

getVariable :: (Monad m) => Identifier -> InterpreterProcess m
getVariable name = do
  s <- get
  fromMaybe (return Null) $ M.lookup name $ variables s

newScope :: (Monad m) => InterpreterProcess m -> InterpreterProcess m
newScope childScope = do
  s <- get
  v <- childScope
  put s
  return v


type InterpreterProcess m = StateT (InterpreterState m) m Value

interpretLanguage :: (Monad m) => Block -> InterpreterProcess m
interpretLanguage = interpretBlock

interpretBlock :: (Monad m) => Block -> InterpreterProcess m
interpretBlock (Block elements) = last <$> mapM interpretElement elements

interpretElement :: (Monad m) => Element -> InterpreterProcess m
interpretElement (ElLoop loop) = interpretLoop loop
interpretElement (ElAssign assignment) = interpretAssignment assignment
interpretElement (ElExpression expression) = interpretExpression expression

-- TODO
-- figure out scoping around function arg blocks
interpretApplication :: (Monad m) => Application -> InterpreterProcess m
interpretApplication (Application name args block) = do
  f <- getVariable name
  case f of
    (Lambda argNames lBlock) ->
      newScope (
        do
          argValues <- mapM interpretExpression args
          zipWithM_ setVariable argNames argValues
          interpretBlock lBlock
      )
    _ -> return Null

interpretLoop :: (Monad m) => Loop -> InterpreterProcess m
interpretLoop (Loop num loopVar block) =
  let
    loopNums = fromIntegral <$> [0..(num-1)]
  in
    foldM_ looping Null loopNums >> return Null
  where
    looping _ n = do
      _ <- maybe (return Null) (\vn -> setVariable vn (Number n)) loopVar
      interpretBlock block

interpretAssignment :: (Monad m) => Assignment -> InterpreterProcess m
interpretAssignment (Assignment name expression) = do
  value <- interpretExpression expression
  setVariable name value

interpretExpression :: (Monad m) => Expression -> InterpreterProcess m
interpretExpression (EApp application) = interpretApplication application
interpretExpression (BinaryOp _ _ _) = undefined
interpretExpression (UnaryOp _ _) = undefined
interpretExpression (EVar (Variable varName)) = getVariable varName
interpretExpression (EVal value) = return value
