module Language
  ( initialState
  , parse
  , interpret
  , createGfx
  , updateStateVariables
  , updateEngineInfo
  , copyRNG
  , module Language.Ast
  ) where

import           Control.Monad                       (forM_)
import           Control.Monad.Except                (runExceptT)
import           Control.Monad.State.Strict          (evalState, execState,
                                                      gets, runState)
import           Control.Monad.Writer.Strict         (runWriterT)
import           System.Random

import           Gfx                                 (Scene (..))
import qualified Gfx.EngineState                     as GE
import           Language.Ast                        (Identifier, Program,
                                                      Value (..))
import           Language.Ast.Transformers.Globalise (globalise)
import           Language.Interpreter                (emptyState,
                                                      getGlobalNames,
                                                      interpretLanguage,
                                                      seedRNG, setVariable)
import           Language.Interpreter.Types          (InterpreterProcess,
                                                      InterpreterState (..),
                                                      currentGfx)
import           Language.Parser                     (parseProgram)
import           Language.StdLib                     (addStdLib)

parse :: String -> Either String Program
parse = parseProgram

initialState :: Int -> [(Identifier, Value)] -> InterpreterState
initialState seed initialVars =
  let setup = do
        addStdLib
        addInitialVariables initialVars
        seedRNG seed
   in execState (runWriterT (runExceptT setup)) emptyState

updateStateVariables ::
     [(Identifier, Value)] -> InterpreterState -> InterpreterState
updateStateVariables vars oldState =
  let setVars = addInitialVariables vars
   in execState (runWriterT (runExceptT setVars)) oldState

updateEngineInfo :: GE.EngineState -> InterpreterState -> InterpreterState
updateEngineInfo es oldState = oldState {engineInfo = GE.createEngineInfo es}

copyRNG :: InterpreterState -> InterpreterState -> InterpreterState
copyRNG oldState newState = newState {rng = rng oldState}

interpret :: [(Identifier, Value)] -> Program -> (Either String Value, [String])
interpret initialVars block =
  let run = do
        addStdLib
        addInitialVariables initialVars
        names <- getGlobalNames
        let ast = globalise names block
        interpretLanguage ast
   in evalState (runWriterT (runExceptT run)) emptyState

createGfx ::
     InterpreterState
  -> Program
  -> ((Either String Scene, [String]), InterpreterState)
createGfx initialState block =
  let run = do
        names <- getGlobalNames
        let ast = globalise names block
        _ <- interpretLanguage ast
        gfxB <- gets currentGfx
        gfxBg <- gets gfxBackground
        gfxAnimStyle <- gets animationStyle
        return
          Scene
            { sceneBackground = gfxBg
            , sceneGfx = gfxB
            , scenePostProcessingFX = gfxAnimStyle
            }
   in runState (runWriterT (runExceptT run)) initialState

addInitialVariables :: [(Identifier, Value)] -> InterpreterProcess ()
addInitialVariables vars = forM_ vars (uncurry setVariable)
