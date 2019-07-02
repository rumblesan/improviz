module Language.StdLib.BlockHandling
  ( addBlockHandlingStdLib
  )
where

import           Language.Ast                   ( Value(Null) )
import           Language.Interpreter           ( execGfx
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types

import           Gfx.Interpreter                ( pushScope
                                                , popScope
                                                )

addBlockHandlingStdLib :: InterpreterProcess ()
addBlockHandlingStdLib = do
  setBuiltIn "pushScope" pushGfxScope
  setBuiltIn "popScope"  popGfxScope

pushGfxScope :: [Value] -> InterpreterProcess Value
pushGfxScope _ = execGfx pushScope >> return Null

popGfxScope :: [Value] -> InterpreterProcess Value
popGfxScope _ = execGfx popScope >> return Null
