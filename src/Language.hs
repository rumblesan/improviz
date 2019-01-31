module Language
  ( initialState
  , parse
  , interpret
  , createGfxScene
  , updateStateVariables
  , module Language.Ast
  ) where

import           Control.Monad               (forM_)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.State.Strict  (evalState, execState, gets,
                                              runState)
import           Control.Monad.Writer.Strict (runWriterT)

import           Gfx                         (Scene (..))
import qualified Gfx.EngineState             as GE
import           Language.Ast                (Identifier, Program, Value (..))
import           Language.Ast.Transformers   (transform)
import           Language.Interpreter        (emptyState, getGlobalNames,
                                              interpretLanguage, setVariable)
import           Language.Interpreter.Types  (InterpreterProcess,
                                              InterpreterState (..), currentGfx)
import           Language.Parser             (parseProgram)
import           Language.StdLib             (addStdLib)

parse :: String -> Either String Program
parse = parseProgram

initialState :: [(Identifier, Value)] -> InterpreterState
initialState initialVars =
  let setup = do
        addStdLib
        addInitialVariables initialVars
   in execState (runWriterT (runExceptT setup)) emptyState

updateStateVariables ::
     [(Identifier, Value)] -> InterpreterState -> InterpreterState
updateStateVariables vars oldState =
  let setVars = addInitialVariables vars
   in execState (runWriterT (runExceptT setVars)) oldState

interpret :: [(Identifier, Value)] -> Program -> (Either String Value, [String])
interpret initialVars program =
  let run = do
        addStdLib
        addInitialVariables initialVars
        globals <- getGlobalNames
        interpretLanguage (transform globals program)
   in evalState (runWriterT (runExceptT run)) emptyState

createGfxScene ::
     InterpreterState
  -> Program
  -> ((Either String Scene, [String]), InterpreterState)
createGfxScene initialState program =
  let run = do
        globals <- getGlobalNames
        _ <- interpretLanguage (transform globals program)
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
