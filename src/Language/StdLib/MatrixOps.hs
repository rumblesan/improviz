module Language.StdLib.MatrixOps (
  rotate, scale, move
) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Language.Interpreter.Types
import Language.Interpreter (addGfxCommand, getVariable, getVariableWithBackup)
import Language.Interpreter.Values
import Language.LanguageAst
import qualified Gfx.GfxAst as GA

import Language.StdLib.BlockHandling (handleGfxBlock)


rotate :: Maybe Block -> InterpreterProcess Value
rotate block = do
    a <- getVariableWithBackup "a" "time" >>= getNumberValue
    b <- getVariableWithBackup "b" "time" >>= getNumberValue
    c <- getVariableWithBackup "c" "time" >>= getNumberValue
    let partialCmd = GA.MatrixCommand $ GA.Rotate a b c
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

scale :: Maybe Block -> InterpreterProcess Value
scale block = do
    a <- getVariable "a" >>= getNumberValue
    b <- getVariable "b" >>= getNumberValue
    c <- getVariable "c" >>= getNumberValue
    let partialCmd = GA.MatrixCommand $ GA.Scale a b c
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

move :: Maybe Block -> InterpreterProcess Value
move block = do
    a <- getVariable "a" >>= getNumberValue
    b <- getVariable "b" >>= getNumberValue
    c <- getVariable "c" >>= getNumberValue
    let partialCmd = GA.MatrixCommand $ GA.Move a b c
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null
