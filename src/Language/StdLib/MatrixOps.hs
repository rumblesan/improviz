module Language.StdLib.MatrixOps
  ( addMatrixStdLib
  )
where

import           Control.Monad.Except           ( throwError )

import           Gfx.Ast                        ( GfxCommand(MatrixCommand)
                                                , MatrixGfx(..)
                                                )
import           Language.Ast                   ( FuncArg(VarArg)
                                                , Value(Symbol, Null)
                                                )
import           Language.Interpreter           ( addGfxCommand
                                                , getNumberFromNull
                                                , getVariableWithError
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )

addMatrixStdLib :: InterpreterProcess ()
addMatrixStdLib = setBuiltIn
  "matrix"
  gfxMatrix
  [VarArg "name", VarArg "x", VarArg "y", VarArg "z"]


gfxMatrix :: InterpreterProcess Value
gfxMatrix = do
  name <- getVariableWithError "name" "Must give matrix function name argument"
  xV   <- getVariableWithError "x" "Must give matrix function an x argument"
  yV   <- getVariableWithError "y" "Must give matrix function a y argument"
  zV   <- getVariableWithError "z" "Must give matrix function a z argument"
  let x = getNumberFromNull xV 0
  let y = getNumberFromNull yV 0
  let z = getNumberFromNull zV 0
  case name of
    (Symbol "rotate") -> addGfxCommand $ MatrixCommand $ Rotate x y z
    (Symbol "scale" ) -> addGfxCommand $ MatrixCommand $ Scale x y z
    (Symbol "move"  ) -> addGfxCommand $ MatrixCommand $ Move x y z
    (Symbol n       ) -> throwError $ "unrecognised matrix (" ++ n ++ ")"
    _                 -> throwError "invalid matrix command value"
  return Null
