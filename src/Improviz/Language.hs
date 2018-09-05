{-# LANGUAGE TemplateHaskell #-}

module Improviz.Language where

import           Language                   (initialState, parse)
import           Language.Ast               (Block (..))
import           Language.Interpreter.Types (InterpreterState)
import           Lens.Simple

data ImprovizLanguage = ImprovizLanguage
  { _programText        :: String
  , _lastProgramText    :: String
  , _currentAst         :: Block
  , _lastWorkingAst     :: Block
  , _initialInterpreter :: InterpreterState
  }

makeLenses ''ImprovizLanguage

makeLanguageState :: Int -> ImprovizLanguage
makeLanguageState seed =
  ImprovizLanguage
    { _programText = ""
    , _lastProgramText = ""
    , _currentAst = Block []
    , _lastWorkingAst = Block []
    , _initialInterpreter = initialState seed []
    }

updateProgram :: String -> Block -> ImprovizLanguage -> ImprovizLanguage
updateProgram newProgram newAst =
  set programText newProgram . set currentAst newAst

resetProgram :: ImprovizLanguage -> ImprovizLanguage
resetProgram as =
  let oldAst = view lastWorkingAst as
      oldText = view lastProgramText as
   in set programText oldText $ set currentAst oldAst as

saveProgram :: ImprovizLanguage -> ImprovizLanguage
saveProgram as =
  let ast = view currentAst as
      text = view programText as
   in set lastWorkingAst ast $ set lastProgramText text as

programHasChanged :: ImprovizLanguage -> Bool
programHasChanged as = as ^. currentAst /= as ^. lastWorkingAst

updateInterpreterState ::
     (InterpreterState -> InterpreterState)
  -> ImprovizLanguage
  -> ImprovizLanguage
updateInterpreterState = over initialInterpreter
