{-# LANGUAGE TemplateHaskell #-}

module Improviz.Language
  ( ImprovizLanguage
  , makeLanguageState
  , initialInterpreter
  , impVMState
  , currentAst
  , updateProgram
  , resetProgram
  , saveProgram
  , programHasChanged
  )
where

import qualified Data.Map.Strict               as M

import           Language                       ( initialState )
import           Language.Ast                   ( Program(..) )
import           Language.Interpreter.Types     ( InterpreterState )
import           Language.ImpVM.Types           ( VMState )
import           Language.ImpVM                 ( cleanVM )
import           Lens.Simple                    ( (^.)
                                                , set
                                                , view
                                                , makeLenses
                                                )
import           Gfx.Context                    ( GfxContext )


data ImprovizLanguage gfxContext = ImprovizLanguage
  { _programText        :: String
  , _lastProgramText    :: String
  , _currentAst         :: Program
  , _lastWorkingAst     :: Program
  , _initialInterpreter :: InterpreterState
  , _impVMState :: VMState gfxContext
  }

makeLenses ''ImprovizLanguage

makeLanguageState :: [Program] -> GfxContext -> IO (ImprovizLanguage GfxContext)
makeLanguageState userCode ctx = do
  initial <- initialState userCode ctx
  return ImprovizLanguage { _programText        = ""
                          , _lastProgramText    = ""
                          , _currentAst         = Program []
                          , _lastWorkingAst     = Program []
                          , _initialInterpreter = initial
                          , _impVMState         = cleanVM ctx M.empty
                          }

updateProgram :: String -> Program -> ImprovizLanguage eg -> ImprovizLanguage eg
updateProgram newProgram newAst =
  set programText newProgram . set currentAst newAst

resetProgram :: ImprovizLanguage eg -> ImprovizLanguage eg
resetProgram as =
  let oldAst  = view lastWorkingAst as
      oldText = view lastProgramText as
  in  set programText oldText $ set currentAst oldAst as

saveProgram :: ImprovizLanguage eg -> ImprovizLanguage eg
saveProgram as =
  let ast  = view currentAst as
      text = view programText as
  in  set lastWorkingAst ast $ set lastProgramText text as

programHasChanged :: ImprovizLanguage eg -> Bool
programHasChanged as = as ^. currentAst /= as ^. lastWorkingAst
