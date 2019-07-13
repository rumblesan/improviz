module Language.Interpreter.StdLib.BlockHandling
  ( addBlockHandlingStdLib
  )
where

import           Language.Ast                   ( Value(Null) )
import           Language.Interpreter           ( useGfxCtx
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types

import           Gfx.Context                    ( pushScope
                                                , popScope
                                                )

addBlockHandlingStdLib :: InterpreterProcess ()
addBlockHandlingStdLib = do
  setBuiltIn "pushScope" pushGfxScope
  setBuiltIn "popScope"  popGfxScope

pushGfxScope :: [Value] -> InterpreterProcess Value
pushGfxScope _ = useGfxCtx pushScope >> return Null

popGfxScope :: [Value] -> InterpreterProcess Value
popGfxScope _ = useGfxCtx popScope >> return Null
