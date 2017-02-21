module LCLangLite.Interpreter.Types where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import LCLangLite.LanguageAst

import qualified LCLangLite.Interpreter.Scope as LS
import qualified Data.Map.Strict as M
import qualified Gfx.GfxAst as GA

type BuiltInFunction = Maybe Block -> InterpreterProcess Value

type InterpreterProcessing = State InterpreterState
type InterpreterLogging m = WriterT [String] m
type InterpreterErrors m = ExceptT String m
type InterpreterProcess v = InterpreterErrors (InterpreterLogging InterpreterProcessing) v

data InterpreterState = InterpreterState {
  variables :: LS.ScopeStack Identifier (InterpreterProcess Value),
  builtins :: M.Map Identifier BuiltInFunction,
  blockStack :: [Block],
  currentGfx :: GA.Block,
  gfxStack :: [GA.Block]
}
