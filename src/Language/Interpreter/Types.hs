module Language.Interpreter.Types where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import Graphics.Rendering.OpenGL (Color4(..))

import Language.LanguageAst

import qualified Language.Interpreter.Scope as LS
import qualified Data.Map.Strict as M
import qualified Gfx.Ast as GA
import Gfx.PostProcessing (AnimationStyle(..))

type BuiltInFunction = InterpreterProcess Value

type InterpreterProcessing = State InterpreterState
type InterpreterLogging m = WriterT [String] m
type InterpreterErrors m = ExceptT String m
type InterpreterProcess v = InterpreterErrors (InterpreterLogging InterpreterProcessing) v

data InterpreterState = InterpreterState {
  variables :: LS.ScopeStack Identifier Value,
  builtins :: M.Map Identifier BuiltInFunction,
  blockStack :: [Maybe Block],
  gfxBackground :: Color4 Float,
  currentGfx :: GA.Block,
  animationStyle :: AnimationStyle,
  gfxStack :: [GA.Block]
}
