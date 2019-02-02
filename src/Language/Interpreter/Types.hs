module Language.Interpreter.Types
  ( BuiltInFunction(..)
  , UserFunctionDef(..)
  , InterpreterState(..)
  , InterpreterProcess
  , runInterpreterM
  )
where

import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Language.Ast

import qualified Data.Map.Strict               as M
import qualified Gfx.Ast                       as GA
import qualified Gfx.EngineState               as GE
import           Gfx.PostProcessing             ( AnimationStyle(..) )
import           Gfx.Types                      ( Colour )
import qualified Language.Interpreter.Scope    as LS

data BuiltInFunction =
  BuiltInFunction [FuncArg]
                  (InterpreterProcess Value)

data UserFunctionDef =
  UserFunctionDef Identifier
                  [FuncArg]
                  Block

type InterpreterProcessing = State InterpreterState

type InterpreterErrors m = ExceptT String m

type InterpreterProcess v = InterpreterErrors InterpreterProcessing v

data InterpreterState = InterpreterState
  { variables      :: LS.ScopeStack Identifier Value
  , globals        :: M.Map Identifier Value
  , builtins       :: M.Map Identifier BuiltInFunction
  , functions      :: M.Map Identifier UserFunctionDef
  , gfxBackground  :: Colour
  , currentGfx     :: GA.Block
  , animationStyle :: AnimationStyle
  , gfxStack       :: [GA.Block]
  , engineInfo     :: GE.EngineInfo
  }

runInterpreterM
  :: InterpreterProcess a
  -> InterpreterState
  -> (Either String a, InterpreterState)
runInterpreterM op = runState (runExceptT op)
