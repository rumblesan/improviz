module Language.StdLib.BlockHandling
  ( addBlockHandlingStdLib
  )
where

import           Language.Ast                   ( Block
                                                , Value(Null)
                                                )
import           Language.Interpreter           ( addGfxCommand
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types

import qualified Gfx.Ast                       as GA

addBlockHandlingStdLib :: InterpreterProcess ()
addBlockHandlingStdLib = do
  setBuiltIn "pushScope" pushGfxScope
  setBuiltIn "popScope"  popGfxScope

pushGfxScope :: [Value] -> Maybe Block -> InterpreterProcess Value
pushGfxScope _ _ = addGfxCommand (GA.ScopeCommand GA.PushScope) >> return Null

popGfxScope :: [Value] -> Maybe Block -> InterpreterProcess Value
popGfxScope _ _ = addGfxCommand (GA.ScopeCommand GA.PopScope) >> return Null
