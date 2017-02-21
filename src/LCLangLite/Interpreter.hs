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

noop :: BuiltInFunction
noop _ = return Null

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
    variables = LS.setVariable (variables s) name (return val)
  }) >> return val

getVariable :: Identifier -> InterpreterProcess Value
getVariable name = do
  s <- get
  fromMaybe (return Null) $ LS.getVariable (variables s) name

setBuiltIn :: Identifier -> BuiltInFunction -> [Identifier] -> InterpreterProcess ()
setBuiltIn name func argNames = modify (\s -> s {
                                  variables = LS.setVariable (variables s) name (return $ BuiltIn argNames),
                                  builtins = M.insert name func (builtins s)
                                  })

getBuiltIn :: Identifier -> InterpreterProcess BuiltInFunction
getBuiltIn name = do
  s <- get
  return $ fromMaybe noop $ M.lookup name $ builtins s

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
interpretAssignment (Assignment name expression) = do
  value <- interpretExpression expression
  setVariable name value

interpretExpression :: Expression -> InterpreterProcess Value
interpretExpression (EApp application) = interpretApplication application
interpretExpression (BinaryOp _ _ _) = undefined
interpretExpression (UnaryOp _ _) = undefined
interpretExpression (EVar (Variable varName)) = getVariable varName
interpretExpression (EVal value) = return value
