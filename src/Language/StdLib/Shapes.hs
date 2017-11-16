module Language.StdLib.Shapes
  ( box
  , sphere
  , cylinder
  , rectangle
  , line
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

box :: InterpreterProcess Value
box = do
  a <- getVarOrNull "a"
  b <- getVarOrNull "b"
  c <- getVarOrNull "c"
  (xSize, ySize, zSize) <-
    case (a, b, c) of
      (Null, Null, Null) -> return (1, 1, 1)
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, y, 1)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to box"
  let partialCmd = GA.ShapeCommand $ GA.Cube xSize ySize zSize
  block <- getBlock
  maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
  return Null

sphere :: InterpreterProcess Value
sphere = do
  a <- getVarOrNull "a"
  b <- getVarOrNull "b"
  c <- getVarOrNull "c"
  (xSize, ySize, zSize) <-
    case (a, b, c) of
      (Null, Null, Null) -> return (1, 1, 1)
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, y, 1)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to sphere"
  let partialCmd = GA.ShapeCommand $ GA.Sphere xSize ySize zSize
  block <- getBlock
  maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
  return Null

cylinder :: InterpreterProcess Value
cylinder = do
  a <- getVarOrNull "a"
  b <- getVarOrNull "b"
  c <- getVarOrNull "c"
  (xSize, ySize, zSize) <-
    case (a, b, c) of
      (Null, Null, Null) -> return (1, 1, 1)
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, y, 1)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to cylinder"
  let partialCmd = GA.ShapeCommand $ GA.Cylinder xSize ySize zSize
  block <- getBlock
  maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
  return Null

rectangle :: InterpreterProcess Value
rectangle = do
  a <- getVarOrNull "a"
  b <- getVarOrNull "b"
  (xSize, ySize) <-
    case (a, b) of
      (Null, Null)         -> return (1, 1)
      (Number x, Null)     -> return (x, x)
      (Number x, Number y) -> return (x, y)
      _                    -> throwError "Error with functions to rectangle"
  let partialCmd = GA.ShapeCommand $ GA.Rectangle xSize ySize
  block <- getBlock
  maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
  return Null

line :: InterpreterProcess Value
line = do
  l <- getVariableWithDefault "l" (Number 1) >>= getNumberValue
  let partialCmd = GA.ShapeCommand $ GA.Line l
  block <- getBlock
  maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
  return Null
