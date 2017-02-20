module LCLangLite.Interpreter.Types where

import LCLangLite.LanguageAst
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified LCLangLite.Interpreter.Scope as LS
import qualified Data.Map.Strict as M
import qualified Gfx.GfxAst as GA

type BuiltInFunction m = Maybe Block -> InterpreterProcess m Value

type InterpreterProcessing m = StateT (InterpreterState m) m
type InterpreterLogging m = WriterT [String] m
type InterpreterProcess m v = InterpreterLogging (InterpreterProcessing m) v

data InterpreterState m = InterpreterState {
  variables :: LS.ScopeStack Identifier (InterpreterProcess m Value),
  builtins :: M.Map Identifier (BuiltInFunction m),
  blockStack :: [Block],
  currentGfx :: GA.Block,
  gfxStack :: [GA.Block]
}
