module Language.Compiler.State where

import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State.Strict
                                                ( State
                                                , get
                                                , gets
                                                , modify
                                                , put
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )

import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )

import           Language.Interpreter.Scope     ( ScopeStack )
import qualified Language.Interpreter.Scope    as SS

data ImpCompilerState = ImpCompilerState
  { nextAddress       :: Int
  , variableAddresses :: ScopeStack String Int
  , functionTable :: M.Map String Int
  , programPosition :: Int
  }

type ImpCompiler = ExceptT String (State ImpCompilerState)

getAddress :: String -> ImpCompiler (Maybe Int)
getAddress name =
  lift $ gets (\st -> SS.getVariable (variableAddresses st) name)

assignAddress :: String -> ImpCompiler Int
assignAddress name = lift $ do
  st <- get
  let addr  = nextAddress st
  let stack = variableAddresses st
  put st { nextAddress       = addr + 1
         , variableAddresses = SS.setVariable stack name addr
         }
  return addr

assignAnonAddress :: ImpCompiler Int
assignAnonAddress = do
  addrNum <- lift $ gets nextAddress
  let name = "<*>anon" ++ show addrNum
  assignAddress name

pushVariableScope :: ImpCompiler ()
pushVariableScope = lift $ modify
  (\st -> st { variableAddresses = SS.newScope $ variableAddresses st })

popVariableScope :: ImpCompiler ()
popVariableScope = do
  prevScope <- lift $ gets (SS.popScope . variableAddresses)
  case prevScope of
    Right ns  -> lift $ modify (\st -> st { variableAddresses = ns })
    Left  err -> throwError err


markFunction :: String -> ImpCompiler ()
markFunction name = lift $ do
  position <- gets programPosition
  ft       <- gets functionTable
  let new = M.insert name position ft
  modify (\st -> st { functionTable = new })

functionAddress :: String -> ImpCompiler Int
functionAddress name =
  lift $ fromMaybe 0 . M.lookup name <$> gets functionTable

incrementPosition :: Int -> ImpCompiler ()
incrementPosition delta = lift $ do
  current <- gets programPosition
  let new = current + delta
  modify (\st -> st { programPosition = new })

emptyTransformState :: ImpCompilerState
emptyTransformState = ImpCompilerState { nextAddress       = 1
                                       , variableAddresses = SS.empty
                                       , functionTable     = M.empty
                                       , programPosition   = 0
                                       }
