module Language.StdLib.MatrixOps
  ( addMatrixStdLib
  )
where

import           Control.Monad.Except           ( throwError )

import           Gfx.Interpreter                ( rotate
                                                , scale
                                                , move
                                                )
import           Language.Ast                   ( Value(Symbol, Null, Number) )
import           Language.Interpreter           ( execGfx
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )

addMatrixStdLib :: InterpreterProcess ()
addMatrixStdLib = setBuiltIn "matrix" gfxMatrix


gfxMatrix :: [Value] -> InterpreterProcess Value
gfxMatrix args = do
  case args of
    [name, Number x, Number y, Number z] -> case name of
      (Symbol "rotate") -> execGfx $ rotate x y z
      (Symbol "scale" ) -> execGfx $ scale x y z
      (Symbol "move"  ) -> execGfx $ move x y z
      (Symbol n       ) -> throwError $ "unrecognised matrix (" ++ n ++ ")"
      _                 -> throwError "invalid matrix command value"
    _ -> throwError "invalid arguments given to matrix"
  return Null
