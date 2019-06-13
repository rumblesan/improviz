module Language.ImpVM.Types where

import qualified Data.Map                      as M
import           Data.Vector                    ( Vector )
import           Control.Monad.State            ( StateT )

data StackItem = SFloat Float | SString String | SNull deriving (Show, Eq)

type VM externalState a = StateT (VMState externalState) IO a

data Op = AddOp | SubOp | MultOp | DivOp | EQOp | NEQOp deriving (Show)

newtype ImpVMError = ImpVMError String deriving (Show)

data Instruction
  = Push StackItem
  | Pop
  | Operator Op
  | Jump Int
  | RelJump Int
  | Branch Int
  | Constant StackItem
  | External String
  | Load Int
  | Save Int
  | Call Int Int
  | BuiltIn String Int
  | Return
  | End deriving (Show)

data VMState externalState = VMState
  { _programCounter :: Int
  , _program :: Vector Instruction
  , _opstack :: [StackItem]
  , _callstack :: [Int]
  , _memory :: Vector StackItem
  , _builtins :: M.Map String (VM externalState ())
  , _running :: Bool
  , _vmError :: Maybe ImpVMError
  , _externalVars :: M.Map String StackItem
  , _externalState :: externalState
  }
