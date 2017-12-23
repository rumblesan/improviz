{-# LANGUAGE TemplateHaskell #-}

module AppState where

import           Language                   (initialState, parse)
import           Language.Ast               (Block (..))
import           Language.Interpreter.Types (InterpreterState)
import           Lens.Simple

data AppState = AppState
  { _displayText        :: Bool
  , _programText        :: String
  , _lastProgramText    :: String
  , _currentAst         :: Block
  , _lastWorkingAst     :: Block
  , _initialInterpreter :: InterpreterState
  , _startTime          :: Float
  }

makeLenses ''AppState

makeAppState :: Float -> AppState
makeAppState start =
  AppState
  { _displayText = True
  , _programText = ""
  , _lastProgramText = ""
  , _currentAst = Block []
  , _lastWorkingAst = Block []
  , _initialInterpreter = initialState []
  , _startTime = start
  }

updateProgram :: String -> Block -> AppState -> AppState
updateProgram newProgram newAst =
  set programText newProgram . set currentAst newAst

toggleText :: AppState -> AppState
toggleText = over displayText not

nudgeBeat :: Float -> AppState -> AppState
nudgeBeat amount = over startTime (+ amount)

resetProgram :: AppState -> AppState
resetProgram as =
  let oldAst = view lastWorkingAst as
      oldText = view lastProgramText as
  in set programText oldText $ set currentAst oldAst as

saveProgram :: AppState -> AppState
saveProgram as =
  let ast = view currentAst as
      text = view programText as
  in set lastWorkingAst ast $ set lastProgramText text as

programHasChanged :: AppState -> Bool
programHasChanged as = view currentAst as == view lastWorkingAst as

getBeat :: Float -> AppState -> Float
getBeat t as = (t - view startTime as) / 60.0
