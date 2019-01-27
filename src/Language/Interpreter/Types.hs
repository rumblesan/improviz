module Language.Interpreter.Types where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

import           Language.Ast

import qualified Data.Map.Strict             as M
import qualified Gfx.Ast                     as GA
import qualified Gfx.EngineState             as GE
import           Gfx.PostProcessing          (AnimationStyle (..))
import           Gfx.Types                   (Colour)
import qualified Language.Interpreter.Scope  as LS

data BuiltInFunction =
  BuiltInFunction [Identifier]
                  (InterpreterProcess Value)

data UserFunctionDef =
  UserFunctionDef Identifier
                  [Identifier]
                  Block

type InterpreterProcessing = State InterpreterState

type InterpreterLogging m = WriterT [String] m

type InterpreterErrors m = ExceptT String m

type InterpreterProcess v
   = InterpreterErrors (InterpreterLogging InterpreterProcessing) v

data InterpreterState = InterpreterState
  { variables      :: LS.ScopeStack Identifier Value
  , globals        :: M.Map Identifier Value
  , builtins       :: M.Map Identifier BuiltInFunction
  , functions      :: M.Map Identifier UserFunctionDef
  , blockStack     :: [Maybe Block]
  , gfxBackground  :: Colour
  , currentGfx     :: GA.Block
  , animationStyle :: AnimationStyle
  , gfxStack       :: [GA.Block]
  , engineInfo     :: GE.EngineInfo
  }
