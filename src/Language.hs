module Language (
  parse,
  interpret,
  createGfx,
  module Language.LanguageAst
  ) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import Language.LanguageParser (parseProgram)
import Language.LanguageAst (Identifier)
import Language.Interpreter.Types (currentGfx, InterpreterProcess)
import Language.Interpreter (interpretLanguage, emptyState, setBuiltIn, setVariable)
import Language.LanguageAst (Block, Value)
import qualified Language.StdLib as SL
import qualified Gfx.Ast as GA

parse :: String -> Maybe Block
parse program = case parseProgram program of
  Left _ -> Nothing
  Right ast -> Just ast

interpret :: [(Identifier, Value)] -> Block -> (Either String Value, [String])
interpret initialVars block =
  let
     run = do
       addStdLib
       addInitialVariables initialVars
       interpretLanguage block
  in
    evalState (runWriterT (runExceptT run)) emptyState

createGfx :: [(Identifier, Value)] -> Block -> (Either String GA.Block, [String])
createGfx initialVars block =
  let
     run = do
       addStdLib
       addInitialVariables initialVars
       _ <- interpretLanguage block
       gets currentGfx
  in
    evalState (runWriterT (runExceptT run)) emptyState

addStdLib :: InterpreterProcess ()
addStdLib = do
  setBuiltIn "noop" SL.noop []
  setBuiltIn "box" SL.box ["a", "b", "c"]
  setBuiltIn "sphere" SL.sphere ["a", "b", "c"]
  setBuiltIn "cylinder" SL.cylinder ["a", "b", "c"]
  setBuiltIn "rectangle" SL.rectangle ["a", "b"]
  setBuiltIn "line" SL.line ["a"]
  setBuiltIn "rotate" SL.rotate ["a", "b", "c"]
  setBuiltIn "scale" SL.scale ["a", "b", "c"]
  setBuiltIn "move" SL.move ["a", "b", "c"]
  setBuiltIn "fill" SL.fill ["r", "g", "b", "a"]
  setBuiltIn "noFill" SL.noFill []
  setBuiltIn "stroke" SL.stroke ["r", "g", "b", "a"]
  setBuiltIn "noStroke" SL.noStroke []

addInitialVariables :: [(Identifier, Value)] -> InterpreterProcess()
addInitialVariables vars = forM_ vars (uncurry setVariable)
