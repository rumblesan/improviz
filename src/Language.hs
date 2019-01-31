module Language
  ( initialState
  , parse
  , interpret
  , createGfxScene
  , updateStateVariables
  , module Language.Ast
  ) where

import           Control.Monad              (forM_)
import           Control.Monad.State.Strict (gets)

import           Gfx                        (Scene (..))

import           Language.Ast               (Identifier, Program, Value (..))
import           Language.Ast.Transformers  (transform)
import           Language.Interpreter       (emptyState, getGlobalNames,
                                             interpretLanguage, setVariable)
import           Language.Interpreter.Types (InterpreterProcess,
                                             InterpreterState (..), currentGfx,
                                             runInterpreterM)
import           Language.Parser            (parseProgram)
import           Language.StdLib            (addStdLib)

parse :: String -> Either String Program
parse = parseProgram

initialState :: [(Identifier, Value)] -> InterpreterState
initialState initialVars =
  let setup = do
        addStdLib
        addInitialVariables initialVars
   in snd $ runInterpreterM setup emptyState

updateStateVariables ::
     [(Identifier, Value)] -> InterpreterState -> InterpreterState
updateStateVariables vars oldState =
  let setVars = addInitialVariables vars
   in snd $ runInterpreterM setVars oldState

interpret :: [(Identifier, Value)] -> Program -> Either String Value
interpret initialVars program =
  let run = do
        addStdLib
        addInitialVariables initialVars
        globals <- getGlobalNames
        interpretLanguage (transform globals program)
   in fst $ runInterpreterM run emptyState

createGfxScene ::
     InterpreterState -> Program -> (Either String Scene, InterpreterState)
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
   in runInterpreterM run initialState

addInitialVariables :: [(Identifier, Value)] -> InterpreterProcess ()
addInitialVariables vars = forM_ vars (uncurry setVariable)
