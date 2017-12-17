module Language
  ( parse
  , interpret
  , createGfx
  , module Language.Ast
  ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

import           Gfx                         (Scene (..))
import           Language.Ast                (Block, Identifier, Value (..))
import           Language.Interpreter        (emptyState, interpretLanguage,
                                              setBuiltIn, setVariable)
import           Language.Interpreter.Types  (InterpreterProcess,
                                              InterpreterState (..), currentGfx)
import           Language.Parser             (parseProgram)
import           Language.StdLib             (addStdLib)

parse :: String -> Either String Block
parse = parseProgram

interpret :: [(Identifier, Value)] -> Block -> (Either String Value, [String])
interpret initialVars block =
  let run = do
        addStdLib
        addInitialVariables initialVars
        interpretLanguage block
  in evalState (runWriterT (runExceptT run)) emptyState

createGfx :: [(Identifier, Value)] -> Block -> (Either String Scene, [String])
createGfx initialVars block =
  let run = do
        addStdLib
        addInitialVariables initialVars
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
  in evalState (runWriterT (runExceptT run)) emptyState

addInitialVariables :: [(Identifier, Value)] -> InterpreterProcess ()
addInitialVariables vars = forM_ vars (uncurry setVariable)
