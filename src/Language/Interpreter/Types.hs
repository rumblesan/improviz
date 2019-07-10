{-# LANGUAGE TemplateHaskell #-}

module Language.Interpreter.Types
  ( BuiltInFunction(..)
  , UserFunctionDef(..)
  , InterpreterState(..)
  , variables
  , externals
  , globals
  , builtins
  , functions
  , textureInfo
  , gfxContext
  , InterpreterProcess
  , runInterpreterM
  )
where

import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Lens.Simple                    ( makeLenses )

import           Language.Ast

import qualified Data.Map.Strict               as M
import           Gfx.Context                    ( GfxContext )
import           Gfx.Textures                   ( TextureInfo(..) )
import qualified Language.Interpreter.Scope    as LS

newtype BuiltInFunction =
  BuiltInFunction ([Value] -> InterpreterProcess Value)

data UserFunctionDef =
  UserFunctionDef Identifier
                  [FuncArg]
                  Block

type InterpreterProcessing = StateT InterpreterState IO

type InterpreterErrors m = ExceptT String m

type InterpreterProcess v = InterpreterErrors InterpreterProcessing v

data InterpreterState = InterpreterState
  { _variables   :: LS.ScopeStack Identifier Value
  , _externals :: M.Map Identifier Value
  , _globals     :: M.Map Identifier Value
  , _builtins    :: M.Map Identifier BuiltInFunction
  , _functions   :: M.Map Identifier UserFunctionDef
  , _textureInfo :: TextureInfo
  , _gfxContext  ::  GfxContext
  }

makeLenses ''InterpreterState

runInterpreterM
  :: InterpreterProcess a
  -> InterpreterState
  -> IO (Either String a, InterpreterState)
runInterpreterM op = runStateT (runExceptT op)
