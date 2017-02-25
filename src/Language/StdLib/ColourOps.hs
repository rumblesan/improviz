module Language.StdLib.ColourOps (
  fill, noFill, stroke, noStroke
) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Language.Interpreter.Types
import Language.Interpreter (addGfxCommand, getVariable, getVariableWithDefault)
import Language.Interpreter.Values
import Language.LanguageAst
import qualified Gfx.Ast as GA

import Language.StdLib.BlockHandling (handleGfxBlock)


fill :: Maybe Block -> InterpreterProcess Value
fill block = do
    r <- getVariableWithDefault "r" (Number 1) >>= getNumberValue
    g <- getVariableWithDefault "g" (Number 1) >>= getNumberValue
    b <- getVariableWithDefault "b" (Number 1) >>= getNumberValue
    a <- getVariableWithDefault "a" (Number 1) >>= getNumberValue
    let partialCmd = GA.ColourCommand $ GA.Fill r g b a
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

noFill :: Maybe Block -> InterpreterProcess Value
noFill block = do
    let partialCmd = GA.ColourCommand GA.NoFill
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

stroke :: Maybe Block -> InterpreterProcess Value
stroke block = do
    r <- getVariableWithDefault "r" (Number 1) >>= getNumberValue
    g <- getVariableWithDefault "g" (Number 1) >>= getNumberValue
    b <- getVariableWithDefault "b" (Number 1) >>= getNumberValue
    a <- getVariableWithDefault "a" (Number 1) >>= getNumberValue
    let partialCmd = GA.ColourCommand $ GA.Stroke r g b a
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

noStroke :: Maybe Block -> InterpreterProcess Value
noStroke block = do
    let partialCmd = GA.ColourCommand GA.NoStroke
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null
