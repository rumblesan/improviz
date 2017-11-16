module Language.StdLib.MatrixOps
  ( rotate
  , scale
  , move
  ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

import qualified Gfx.Ast                       as GA
import           Language.Ast
import           Language.Interpreter          (addGfxCommand, getBlock,
                                                getVarOrNull,
                                                getVariableWithDefault)
import           Language.Interpreter.Types
import           Language.Interpreter.Values

import           Language.StdLib.BlockHandling (handleGfxBlock)

rotate :: InterpreterProcess Value
rotate = do
  a <- getVarOrNull "a"
  b <- getVarOrNull "b"
  c <- getVarOrNull "c"
  (xRot, yRot, zRot) <-
    case (a, b, c) of
      (Null, Null, Null) -> do
        time <- getVariableWithDefault "time" (Number 0) >>= getNumberValue
        return (time / 4 * pi, time / 4 * pi, 0)
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, y, 0)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to rotate"
  let partialCmd = GA.MatrixCommand $ GA.Rotate xRot yRot zRot
  block <- getBlock
  maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
  return Null

scale :: InterpreterProcess Value
scale = do
  a <- getVarOrNull "a"
  b <- getVarOrNull "b"
  c <- getVarOrNull "c"
  (xScl, yScl, zScl) <-
    case (a, b, c) of
      (Null, Null, Null) -> do
        time <- getVariableWithDefault "time" (Number 0) >>= getNumberValue
        return (cos time, cos time, cos time)
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, y, 1)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to scale"
  let partialCmd = GA.MatrixCommand $ GA.Scale xScl yScl zScl
  block <- getBlock
  maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
  return Null

move :: InterpreterProcess Value
move = do
  a <- getVarOrNull "a"
  b <- getVarOrNull "b"
  c <- getVarOrNull "c"
  (xMov, yMov, zMov) <-
    case (a, b, c) of
      (Null, Null, Null) -> do
        time <- getVariableWithDefault "time" (Number 0) >>= getNumberValue
        return (cos (2 * pi * time), sin (2 * pi * time), cos (2 * pi * time))
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, y, 0)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to move"
  let partialCmd = GA.MatrixCommand $ GA.Move xMov yMov zMov
  block <- getBlock
  maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
  return Null
