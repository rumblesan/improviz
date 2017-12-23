module Language
  ( initialState
  , parse
  , interpret
  , createGfx
  , updateStateVariables
  , module Language.Ast
  ) where

import           Control.Monad               (forM_)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.State.Strict  (evalState, execState, gets)
import           Control.Monad.Writer.Strict (runWriterT)

import           Gfx                         (Scene (..))
import           Language.Ast                (Block, Identifier, Value (..))
import           Language.Interpreter        (emptyState, interpretLanguage,
                                              setVariable)
import           Language.Interpreter.Types  (InterpreterProcess,
                                              InterpreterState (..), currentGfx)
import           Language.Parser             (parseProgram)
import           Language.StdLib             (addStdLib)

parse :: String -> Either String Block
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

interpret :: [(Identifier, Value)] -> Block -> (Either String Value, [String])
interpret initialVars block =
  let run = do
        addStdLib
        addInitialVariables initialVars
        interpretLanguage block
  in evalState (runWriterT (runExceptT run)) emptyState

createGfx :: InterpreterState -> Block -> Either String Scene
createGfx initialState block =
  let run = do
        _ <- interpretLanguage block
        gfxB <- gets currentGfx
        gfxBg <- gets gfxBackground
        gfxAnimStyle <- gets animationStyle
        return
          Scene
          { sceneBackground = gfxBg
          , sceneGfx = gfxB
          , scenePostProcessingFX = gfxAnimStyle
          }
      (result, logs) = evalState (runWriterT (runExceptT run)) initialState
  in result

addInitialVariables :: [(Identifier, Value)] -> InterpreterProcess ()
addInitialVariables vars = forM_ vars (uncurry setVariable)
