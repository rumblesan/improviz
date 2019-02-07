module Language.StdLib.MatrixOps
  ( addMatrixStdLib
  )
where

import           Control.Monad.Except           ( throwError )

import           Gfx.Ast                        ( GfxCommand(MatrixCommand)
                                                , MatrixGfx(..)
                                                )
import           Language.Ast                   ( Block
                                                , Value(Symbol, Null, Number)
                                                )
import           Language.Interpreter           ( addGfxCommand
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )

addMatrixStdLib :: InterpreterProcess ()
addMatrixStdLib = setBuiltIn "matrix" gfxMatrix


gfxMatrix :: [Value] -> Maybe Block -> InterpreterProcess Value
gfxMatrix args _ = do
  case args of
    [name, Number x, Number y, Number z] -> case name of
      (Symbol "rotate") -> addGfxCommand $ MatrixCommand $ Rotate x y z
      (Symbol "scale" ) -> addGfxCommand $ MatrixCommand $ Scale x y z
      (Symbol "move"  ) -> addGfxCommand $ MatrixCommand $ Move x y z
      (Symbol n       ) -> throwError $ "unrecognised matrix (" ++ n ++ ")"
      _                 -> throwError "invalid matrix command value"
    _ -> throwError "invalid arguments given to matrix"
  return Null
