module Language (
  parse,
  interpret,
  createGfx,
  module Language.Ast
  ) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import Gfx (Scene(..))
import Language.Parser (parseProgram)
import Language.Interpreter.Types (currentGfx, InterpreterProcess, InterpreterState(..))
import Language.Interpreter (interpretLanguage, emptyState, setBuiltIn, setVariable)
import Language.Ast (Block, Value(..), Identifier)
import qualified Language.StdLib as SL

parse :: String -> Either String Block
parse = parseProgram

interpret :: [(Identifier, Value)] -> Block -> (Either String Value, [String])
interpret initialVars block =
  let
     run = do
       addStdLib
       addInitialVariables initialVars
       interpretLanguage block
  in
    evalState (runWriterT (runExceptT run)) emptyState

createGfx :: [(Identifier, Value)] -> Block -> (Either String Scene, [String])
createGfx initialVars block =
  let
     run = do
       addStdLib
       addInitialVariables initialVars
       _ <- interpretLanguage block
       gfxB <- gets currentGfx
       gfxBg <- gets gfxBackground
       gfxAnimStyle <- gets animationStyle
       return Scene {
         sceneBackground = gfxBg,
         sceneGfx = gfxB,
         scenePostProcessingFX = gfxAnimStyle
       }
  in
    evalState (runWriterT (runExceptT run)) emptyState

addStdLib :: InterpreterProcess ()
addStdLib = do
  setBuiltIn "noop" SL.noop []
  setBuiltIn "runBlock" SL.runBlock []
  setBuiltIn "box" SL.box ["a", "b", "c"]
  setBuiltIn "sphere" SL.sphere ["a", "b", "c"]
  setBuiltIn "cylinder" SL.cylinder ["a", "b", "c"]
  setBuiltIn "rectangle" SL.rectangle ["a", "b"]
  setBuiltIn "line" SL.line ["a"]
  setBuiltIn "rotate" SL.rotate ["a", "b", "c"]
  setBuiltIn "scale" SL.scale ["a", "b", "c"]
  setBuiltIn "move" SL.move ["a", "b", "c"]
  setBuiltIn "fill" SL.fill ["r", "g", "b", "a"]
  setBuiltIn "noFill" SL.noFill []
  setBuiltIn "stroke" SL.stroke ["r", "g", "b", "a"]
  setBuiltIn "noStroke" SL.noStroke []
  setBuiltIn "background" SL.background ["r", "g", "b"]
  setBuiltIn "paintOver" SL.paintOver []
  setBuiltIn "motionBlur" SL.motionBlur []
  setBuiltIn "sin" SL.sinFunc ["rads"]
  setBuiltIn "cos" SL.cosFunc ["rads"]
  setBuiltIn "tan" SL.cosFunc ["rads"]
  setBuiltIn "abs" SL.cosFunc ["val"]
  setBuiltIn "ceil" SL.cosFunc ["val"]
  setBuiltIn "floor" SL.cosFunc ["val"]
  setBuiltIn "round" SL.cosFunc ["val"]
  setBuiltIn "max" SL.cosFunc ["val"]
  setBuiltIn "min" SL.cosFunc ["val"]
  setBuiltIn "log" SL.cosFunc ["val"]
  setBuiltIn "sqrt" SL.cosFunc ["val"]
  setVariable "pi" (Number pi)
  return ()

addInitialVariables :: [(Identifier, Value)] -> InterpreterProcess()
addInitialVariables vars = forM_ vars (uncurry setVariable)
