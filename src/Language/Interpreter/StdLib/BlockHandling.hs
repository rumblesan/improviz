module Language.Interpreter.StdLib.BlockHandling
  ( addBlockHandlingStdLib
  )
where

import           Language.Ast                   ( Value(Null) )
import           Language.Interpreter.Types     ( InterpreterProcess
                                                , setBuiltIn
                                                , withGfxCtx
                                                )

import           Gfx.Context                    ( pushScope
                                                , popScope
                                                )

addBlockHandlingStdLib :: InterpreterProcess ()
addBlockHandlingStdLib = do
  setBuiltIn "pushScope" pushGfxScope
  setBuiltIn "popScope"  popGfxScope

pushGfxScope :: [Value] -> InterpreterProcess Value
pushGfxScope _ = withGfxCtx pushScope >> return Null

popGfxScope :: [Value] -> InterpreterProcess Value
popGfxScope _ = withGfxCtx popScope >> return Null
