{-# LANGUAGE TemplateHaskell #-}

module Improviz.Runtime
  ( ImprovizRuntime
  , makeRuntimeState
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

import           Language                       ( initialInterpreterState )
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


data ImprovizRuntime gfxContext = ImprovizRuntime
  { _programText        :: String
  , _lastProgramText    :: String
  , _currentAst         :: Program
  , _lastWorkingAst     :: Program
  , _initialInterpreter :: InterpreterState
  , _impVMState :: VMState gfxContext
  }

makeLenses ''ImprovizRuntime

makeRuntimeState
  :: [(FilePath, Program)] -> GfxContext -> IO (ImprovizRuntime GfxContext)
makeRuntimeState userCode ctx = do
  initial <- initialInterpreterState userCode ctx
  return ImprovizRuntime { _programText        = ""
                         , _lastProgramText    = ""
                         , _currentAst         = Program []
                         , _lastWorkingAst     = Program []
                         , _initialInterpreter = initial
                         , _impVMState         = cleanVM ctx M.empty
                         }

updateProgram :: String -> Program -> ImprovizRuntime eg -> ImprovizRuntime eg
updateProgram newProgram newAst =
  set programText newProgram . set currentAst newAst

resetProgram :: ImprovizRuntime eg -> ImprovizRuntime eg
resetProgram as =
  let oldAst  = view lastWorkingAst as
      oldText = view lastProgramText as
  in  set programText oldText $ set currentAst oldAst as

saveProgram :: ImprovizRuntime eg -> ImprovizRuntime eg
saveProgram as =
  let ast  = view currentAst as
      text = view programText as
  in  set lastWorkingAst ast $ set lastProgramText text as

programHasChanged :: ImprovizRuntime eg -> Bool
programHasChanged as = as ^. currentAst /= as ^. lastWorkingAst
