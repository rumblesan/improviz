module Language.StdLib.BlockHandling
  ( addBlockHandlingStdLib
  ) where

import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

import           Language.Ast                (Value (..))
import           Language.Interpreter        (addGfxCommand, setBuiltIn)
import           Language.Interpreter.Types

import qualified Gfx.Ast                     as GA

addBlockHandlingStdLib :: InterpreterProcess ()
addBlockHandlingStdLib = do
  setBuiltIn "pushScope" pushGfxScope []
  setBuiltIn "popScope" pushGfxScope []

pushGfxScope :: InterpreterProcess Value
pushGfxScope = addGfxCommand (GA.ScopeCommand GA.PushScope) >> return Null

popGfxScope :: InterpreterProcess Value
popGfxScope = addGfxCommand (GA.ScopeCommand GA.PopScope) >> return Null
