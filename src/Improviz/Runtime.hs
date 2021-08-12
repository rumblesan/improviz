{-# LANGUAGE TemplateHaskell #-}

module Improviz.Runtime
  ( ImprovizRuntime
  , makeRuntimeState
  , initialInterpreter
  , currentAst
  , updateProgram
  , resetProgram
  , saveProgram
  , resizeRuntime
  , programHasChanged
  , materialsToLoad
  ) where

import           Gfx.Context                    ( GfxContext )
import           Gfx.Engine                     ( GfxEngine )
import           Gfx.Materials                  ( MaterialData )
import qualified Improviz.SystemVars           as SV
import           Language                       ( initialInterpreterState
                                                , updateSystemVars
                                                )
import           Language.Ast                   ( Program(..) )
import           Language.Interpreter.Types     ( InterpreterState )
import           Lens.Simple                    ( (^.)
                                                , makeLenses
                                                , set
                                                , view
                                                )


data ImprovizRuntime gfxContext = ImprovizRuntime
  { _programText        :: String
  , _lastProgramText    :: String
  , _currentAst         :: Program
  , _lastWorkingAst     :: Program
  , _materialsToLoad    :: [MaterialData]
  , _initialInterpreter :: InterpreterState
  }

makeLenses ''ImprovizRuntime

makeRuntimeState
  :: [(FilePath, Program)]
  -> GfxEngine
  -> GfxContext
  -> IO (ImprovizRuntime GfxContext)
makeRuntimeState userCode gfx ctx =
  let sysVars = SV.create gfx
  in  do
        is <- initialInterpreterState sysVars userCode ctx
        return ImprovizRuntime { _programText        = ""
                               , _lastProgramText    = ""
                               , _currentAst         = Program []
                               , _lastWorkingAst     = Program []
                               , _materialsToLoad    = []
                               , _initialInterpreter = is
                               }


resizeRuntime
  :: GfxEngine -> ImprovizRuntime GfxContext -> ImprovizRuntime GfxContext
resizeRuntime gfx runtime =
  let newSysVars  = SV.create gfx
      interpState = _initialInterpreter runtime
  in  runtime { _initialInterpreter = updateSystemVars newSysVars interpState }

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
