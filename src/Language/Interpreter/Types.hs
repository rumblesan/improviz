{-# LANGUAGE TemplateHaskell #-}

module Language.Interpreter.Types where

import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad.Except
import           Control.Monad.State.Strict

import           System.Random                  ( StdGen
                                                , mkStdGen
                                                )

import           Lens.Simple                    ( assign
                                                , at
                                                , makeLenses
                                                , use
                                                , uses
                                                , (%=)
                                                )

import           Gfx.Context                    ( GfxContext )
import qualified Gfx.Context                   as GfxC
import           Language.Ast
import qualified Language.Interpreter.Scope    as LS

newtype BuiltInFunction =
  BuiltInFunction ([Value] -> InterpreterProcess Value)

type InterpreterProcessing = StateT InterpreterState IO

type InterpreterErrors m = ExceptT String m

type InterpreterProcess v = InterpreterErrors InterpreterProcessing v

data InterpreterState = InterpreterState
  { _variables   :: LS.ScopeStack Identifier Value
  , _externals   :: M.Map Identifier Value
  , _globals     :: M.Map Identifier Value
  , _systemVars  :: M.Map Identifier Value
  , _builtins    :: M.Map Identifier BuiltInFunction
  , _functions   :: M.Map Identifier Lambda
  , _textureInfo :: M.Map String Int
  , _gfxContext  :: GfxContext
  , _rnGen       :: StdGen
  }

makeLenses ''InterpreterState

empty :: InterpreterState
empty = InterpreterState { _variables   = LS.empty
                         , _externals   = M.empty
                         , _globals     = M.empty
                         , _systemVars  = M.empty
                         , _builtins    = M.empty
                         , _functions   = M.empty
                         , _textureInfo = M.empty
                         , _gfxContext  = GfxC.empty
                         , _rnGen       = mkStdGen 1234
                         }

setVariable :: Identifier -> Value -> InterpreterProcess Value
setVariable name val = do
  variables %= (\varStack -> LS.setVariable varStack name val)
  return val

getVariable :: Identifier -> InterpreterProcess Value
getVariable name = do
  variableDefs <- use variables
  case LS.getVariable variableDefs name of
    Just v  -> return v
    Nothing -> throwError $ "Could not get variable: " ++ name

newScope :: InterpreterProcess Value -> InterpreterProcess Value
newScope scopedBlock = do
  variables %= LS.newScope
  v <- scopedBlock
  variables %= popScope
  return v
 where
  -- FIXME this probably aught to blow up if there's an error really
  popScope :: LS.ScopeStack k v -> LS.ScopeStack k v
  popScope oldS = case LS.popScope oldS of
    Left  _      -> oldS
    Right popped -> popped

setExternals :: M.Map String Value -> InterpreterProcess ()
setExternals = assign externals

getExternal :: Identifier -> Value -> InterpreterProcess Value
getExternal name defaultValue =
  uses (externals . at name) (fromMaybe defaultValue)

setGlobal :: Identifier -> Value -> InterpreterProcess ()
setGlobal name val = assign (globals . at name) (Just val)

getGlobal :: Identifier -> InterpreterProcess Value
getGlobal name = do
  val <- use (globals . at name)
  case val of
    Just v  -> return v
    Nothing -> throwError $ "Could not get global: " ++ name

getGlobalNames :: InterpreterProcess (S.Set String)
getGlobalNames = uses globals M.keysSet

setSystemVars :: [(Identifier, Value)] -> InterpreterProcess ()
setSystemVars sysVars = assign systemVars (M.fromList sysVars)

getSystemVar :: Identifier -> InterpreterProcess Value
getSystemVar name = uses (systemVars . at name) (fromMaybe Null)

setBuiltIn
  :: Identifier
  -> ([Value] -> InterpreterProcess Value)
  -> InterpreterProcess ()
setBuiltIn name func = do
  assign (globals . at name)  (Just $ BuiltInFunctionRef name)
  assign (builtins . at name) (Just $ BuiltInFunction func)

getBuiltIn :: Identifier -> InterpreterProcess BuiltInFunction
getBuiltIn name = do
  builtIn <- use (builtins . at name)
  case builtIn of
    Just b  -> return b
    Nothing -> throwError $ "Could not get builtin: " ++ name

getTextureInfo :: String -> InterpreterProcess (Maybe Int)
getTextureInfo name = use (textureInfo . at name)

withGfxCtx :: (GfxContext -> IO ()) -> InterpreterProcess ()
withGfxCtx action = do
  ctx <- use gfxContext
  liftIO $ action ctx

runInterpreterM
  :: InterpreterProcess a
  -> InterpreterState
  -> IO (Either String a, InterpreterState)
runInterpreterM op = runStateT (runExceptT op)
