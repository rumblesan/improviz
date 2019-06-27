{-# LANGUAGE TemplateHaskell #-}

module Language.Interpreter.Types
  ( BuiltInFunction(..)
  , UserFunctionDef(..)
  , InterpreterState(..)
  , variables
  , globals
  , builtins
  , functions
  , currentGfx
  , textureInfo
  , gfxEngine
  , InterpreterProcess
  , runInterpreterM
  )
where

import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Lens.Simple                    ( makeLenses )

import           Language.Ast

import qualified Data.Map.Strict               as M
import           Gfx.EngineState                ( EngineState )
import qualified Gfx.Ast                       as GA
import           Gfx.Textures                   ( TextureInfo(..) )
import qualified Language.Interpreter.Scope    as LS

newtype BuiltInFunction =
  BuiltInFunction ([Value] -> Maybe Block -> InterpreterProcess Value)

data UserFunctionDef =
  UserFunctionDef Identifier
                  [FuncArg]
                  Block

type InterpreterProcessing = State InterpreterState

type InterpreterErrors m = ExceptT String m

type InterpreterProcess v = InterpreterErrors InterpreterProcessing v

data InterpreterState = InterpreterState
  { _variables      :: LS.ScopeStack Identifier Value
  , _globals        :: M.Map Identifier Value
  , _builtins       :: M.Map Identifier BuiltInFunction
  , _functions      :: M.Map Identifier UserFunctionDef
  , _currentGfx     :: GA.Block
  , _textureInfo    :: TextureInfo
  , _gfxEngine      :: Maybe EngineState
  }

makeLenses ''InterpreterState

runInterpreterM
  :: InterpreterProcess a
  -> InterpreterState
  -> (Either String a, InterpreterState)
runInterpreterM op = runState (runExceptT op)
