module Language.StdLib.MatrixOps
  ( addMatrixStdLib
  )
where

import           Control.Monad.Except           ( throwError )

import qualified Gfx.Ast                       as GA
import           Language.Ast
import           Language.Interpreter           ( addGfxCommand
                                                , getNumberFromNull
                                                , getVariableWithError
                                                , getVarOrNull
                                                , getVariableWithDefault
                                                , gfxScopedBlock
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types
import           Language.Interpreter.Values

addMatrixStdLib :: InterpreterProcess ()
addMatrixStdLib = do
  setBuiltIn "rotate"
             rotate
             [VarArg "x", VarArg "y", VarArg "z", BlockArg "block"]
  setBuiltIn "scale"
             scale
             [VarArg "x", VarArg "y", VarArg "z", BlockArg "block"]
  setBuiltIn "move" move [VarArg "x", VarArg "y", VarArg "z", BlockArg "block"]
  setBuiltIn "matrix"
             gfxMatrix
             [VarArg "name", VarArg "x", VarArg "y", VarArg "z"]

rotate :: InterpreterProcess Value
rotate = do
  xV                 <- getVarOrNull "x"
  yV                 <- getVarOrNull "y"
  zV                 <- getVarOrNull "z"
  blockRef           <- getVarOrNull "block"
  (xRot, yRot, zRot) <- case (xV, yV, zV) of
    (Null, Null, Null) -> do
      time <- getVariableWithDefault "time" (Number 0) >>= getNumberValue
      return (time / 4 * pi, time / 4 * pi, 0)
    (v1, v2, v3) ->
      return
        (getNumberFromNull v1 0, getNumberFromNull v2 0, getNumberFromNull v3 0)
  let cmd = GA.MatrixCommand $ GA.Rotate xRot yRot zRot
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

scale :: InterpreterProcess Value
scale = do
  xV                 <- getVarOrNull "x"
  yV                 <- getVarOrNull "y"
  zV                 <- getVarOrNull "z"
  blockRef           <- getVarOrNull "block"
  (xScl, yScl, zScl) <- case (xV, yV, zV) of
    (Null, Null, Null) -> do
      time <- getVariableWithDefault "time" (Number 0) >>= getNumberValue
      return (cos time, cos time, cos time)
    (v1, v2, v3) ->
      return
        (getNumberFromNull v1 0, getNumberFromNull v2 0, getNumberFromNull v3 0)
  let cmd = GA.MatrixCommand $ GA.Scale xScl yScl zScl
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

move :: InterpreterProcess Value
move = do
  xV                 <- getVarOrNull "x"
  yV                 <- getVarOrNull "y"
  zV                 <- getVarOrNull "z"
  blockRef           <- getVarOrNull "block"
  (xMov, yMov, zMov) <- case (xV, yV, zV) of
    (Null, Null, Null) -> do
      time <- getVariableWithDefault "time" (Number 0) >>= getNumberValue
      return (cos (2 * pi * time), sin (2 * pi * time), cos (2 * pi * time))
    (v1, v2, v3) ->
      return
        (getNumberFromNull v1 0, getNumberFromNull v2 0, getNumberFromNull v3 0)
  let cmd = GA.MatrixCommand $ GA.Move xMov yMov zMov
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

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
    (Symbol "rotate") -> addGfxCommand $ GA.MatrixCommand $ GA.Rotate x y z
    (Symbol "scale" ) -> addGfxCommand $ GA.MatrixCommand $ GA.Scale x y z
    (Symbol "move"  ) -> addGfxCommand $ GA.MatrixCommand $ GA.Move x y z
    (Symbol n       ) -> throwError $ "unrecognised matrix (" ++ n ++ ")"
    _                 -> throwError "invalid matrix command value"
  return Null
