module AppTypes where

import           Language                   (initialState)
import           Language.Ast               (Block (..))
import           Language.Interpreter.Types (InterpreterState)

data AppState = AppState
  { displayText        :: Bool
  , programText        :: String
  , currentAst         :: Block
  , lastWorkingAst     :: Block
  , initialInterpreter :: InterpreterState
  }

makeAppState :: AppState
makeAppState =
  AppState
  { displayText = True
  , programText = ""
  , currentAst = Block []
  , lastWorkingAst = Block []
  , initialInterpreter = initialState []
  }
