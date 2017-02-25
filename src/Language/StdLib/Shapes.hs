module Language.StdLib.Shapes (
  box, sphere, cylinder, rectangle, line
) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Language.Interpreter.Types
import Language.Interpreter (addGfxCommand, getVariableWithDefault)
import Language.Interpreter.Values
import Language.LanguageAst
import qualified Gfx.Ast as GA

import Language.StdLib.BlockHandling (handleGfxBlock)


box :: Maybe Block -> InterpreterProcess Value
box block = do
    a <- getVariableWithDefault "a" (Number 1) >>= getNumberValue
    b <- getVariableWithDefault "b" (Number 1) >>= getNumberValue
    c <- getVariableWithDefault "c" (Number 1) >>= getNumberValue
    let partialCmd = GA.ShapeCommand $ GA.Cube a b c
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

sphere :: Maybe Block -> InterpreterProcess Value
sphere block = do
    a <- getVariableWithDefault "a" (Number 1) >>= getNumberValue
    b <- getVariableWithDefault "b" (Number 1) >>= getNumberValue
    c <- getVariableWithDefault "c" (Number 1) >>= getNumberValue
    let partialCmd = GA.ShapeCommand $ GA.Sphere a b c
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

cylinder :: Maybe Block -> InterpreterProcess Value
cylinder block = do
    a <- getVariableWithDefault "a" (Number 1) >>= getNumberValue
    b <- getVariableWithDefault "b" (Number 1) >>= getNumberValue
    c <- getVariableWithDefault "c" (Number 1) >>= getNumberValue
    let partialCmd = GA.ShapeCommand $ GA.Cylinder a b c
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

rectangle :: Maybe Block -> InterpreterProcess Value
rectangle block = do
    a <- getVariableWithDefault "a" (Number 1) >>= getNumberValue
    b <- getVariableWithDefault "b" (Number 1) >>= getNumberValue
    let partialCmd = GA.ShapeCommand $ GA.Rectangle a b
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

line :: Maybe Block -> InterpreterProcess Value
line block = do
    l <- getVariableWithDefault "l" (Number 1) >>= getNumberValue
    let partialCmd = GA.ShapeCommand $ GA.Line l
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null
