module Language.StdLib.MatrixOps
  ( addMatrixStdLib
  ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

import qualified Gfx.Ast                     as GA
import           Language.Ast
import           Language.Interpreter        (addGfxCommand, getBlock,
                                              getNumberFromNull, getVarOrNull,
                                              getVariableWithDefault,
                                              gfxScopedBlock, setBuiltIn)
import           Language.Interpreter.Types
import           Language.Interpreter.Values

addMatrixStdLib :: InterpreterProcess ()
addMatrixStdLib = do
  setBuiltIn "rotate" rotate ["x", "y", "z"]
  setBuiltIn "scale" scale ["x", "y", "z"]
  setBuiltIn "move" move ["x", "y", "z"]

rotate :: InterpreterProcess Value
rotate = do
  xV <- getVarOrNull "x"
  yV <- getVarOrNull "y"
  zV <- getVarOrNull "z"
  (xRot, yRot, zRot) <-
    case (xV, yV, zV) of
      (Null, Null, Null) -> do
        time <- getVariableWithDefault "time" (Number 0) >>= getNumberValue
        return (time / 4 * pi, time / 4 * pi, 0)
      (v1, v2, v3) ->
        return
          ( getNumberFromNull v1 0
          , getNumberFromNull v2 0
          , getNumberFromNull v3 0)
  let cmd = GA.MatrixCommand $ GA.Rotate xRot yRot zRot
  block <- getBlock
  maybe (addGfxCommand cmd) (gfxScopedBlock cmd) block
  return Null

scale :: InterpreterProcess Value
scale = do
  xV <- getVarOrNull "x"
  yV <- getVarOrNull "y"
  zV <- getVarOrNull "z"
  (xScl, yScl, zScl) <-
    case (xV, yV, zV) of
      (Null, Null, Null) -> do
        time <- getVariableWithDefault "time" (Number 0) >>= getNumberValue
        return (cos time, cos time, cos time)
      (v1, v2, v3) ->
        return
          ( getNumberFromNull v1 0
          , getNumberFromNull v2 0
          , getNumberFromNull v3 0)
  let cmd = GA.MatrixCommand $ GA.Scale xScl yScl zScl
  block <- getBlock
  maybe (addGfxCommand cmd) (gfxScopedBlock cmd) block
  return Null

move :: InterpreterProcess Value
move = do
  xV <- getVarOrNull "x"
  yV <- getVarOrNull "y"
  zV <- getVarOrNull "z"
  (xMov, yMov, zMov) <-
    case (xV, yV, zV) of
      (Null, Null, Null) -> do
        time <- getVariableWithDefault "time" (Number 0) >>= getNumberValue
        return (cos (2 * pi * time), sin (2 * pi * time), cos (2 * pi * time))
      (v1, v2, v3) ->
        return
          ( getNumberFromNull v1 0
          , getNumberFromNull v2 0
          , getNumberFromNull v3 0)
  let cmd = GA.MatrixCommand $ GA.Move xMov yMov zMov
  block <- getBlock
  maybe (addGfxCommand cmd) (gfxScopedBlock cmd) block
  return Null
