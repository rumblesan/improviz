module Language
  ( initialState
  , parse
  , interpret
  , createGfx
  , updateStateVariables
  , module Language.Ast
  ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

import           Gfx                         (Scene (..))
import           Language.Ast                (Block, Identifier, Value (..))
import           Language.Interpreter        (emptyState, interpretLanguage,
                                              setVariable)
import qualified Language.Interpreter.Scope  as LS
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
  let newVarMap = foldl setVar (variables oldState) vars
  in oldState {variables = newVarMap}
  where
    setVar varMap (name, value) = LS.setVariable varMap name value

interpret :: [(Identifier, Value)] -> Block -> (Either String Value, [String])
interpret initialVars block =
  let run = do
        addStdLib
        addInitialVariables initialVars
        interpretLanguage block
  in evalState (runWriterT (runExceptT run)) emptyState

createGfx :: InterpreterState -> Block -> (Either String Scene, [String])
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
  in evalState (runWriterT (runExceptT run)) initialState

addInitialVariables :: [(Identifier, Value)] -> InterpreterProcess ()
addInitialVariables vars = forM_ vars (uncurry setVariable)
