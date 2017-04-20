module Language.StdLib.Shapes (
  box, sphere, cylinder, rectangle, line
) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import Language.Interpreter.Types
import Language.Interpreter (addGfxCommand, getVarOrNull, getVariableWithDefault)
import Language.Interpreter.Values
import Language.LanguageAst
import qualified Gfx.Ast as GA

import Language.StdLib.BlockHandling (handleGfxBlock)


box :: Maybe Block -> InterpreterProcess Value
box block = do
    a <- getVarOrNull "a"
    b <- getVarOrNull "b"
    c <- getVarOrNull "c"
    (xSize, ySize, zSize) <- case (a, b, c) of
      (Null, Null, Null) -> return (1, 1, 1)
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, y, 1)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to box"
    let partialCmd = GA.ShapeCommand $ GA.Cube xSize ySize zSize
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

sphere :: Maybe Block -> InterpreterProcess Value
sphere block = do
    a <- getVarOrNull "a"
    b <- getVarOrNull "b"
    c <- getVarOrNull "c"
    (xSize, ySize, zSize) <- case (a, b, c) of
      (Null, Null, Null) -> return (1, 1, 1)
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, y, 1)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to sphere"
    let partialCmd = GA.ShapeCommand $ GA.Sphere xSize ySize zSize
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

cylinder :: Maybe Block -> InterpreterProcess Value
cylinder block = do
    a <- getVarOrNull "a"
    b <- getVarOrNull "b"
    c <- getVarOrNull "c"
    (xSize, ySize, zSize) <- case (a, b, c) of
      (Null, Null, Null) -> return (1, 1, 1)
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, y, 1)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to cylinder"
    let partialCmd = GA.ShapeCommand $ GA.Cylinder xSize ySize zSize
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

rectangle :: Maybe Block -> InterpreterProcess Value
rectangle block = do
    a <- getVarOrNull "a"
    b <- getVarOrNull "b"
    (xSize, ySize) <- case (a, b) of
      (Null, Null) -> return (1, 1)
      (Number x, Null) -> return (x, x)
      (Number x, Number y) -> return (x, y)
      _ -> throwError "Error with functions to rectangle"
    let partialCmd = GA.ShapeCommand $ GA.Rectangle xSize ySize
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

line :: Maybe Block -> InterpreterProcess Value
line block = do
    l <- getVariableWithDefault "l" (Number 1) >>= getNumberValue
    let partialCmd = GA.ShapeCommand $ GA.Line l
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

