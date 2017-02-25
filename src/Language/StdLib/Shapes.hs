module Language.StdLib.Shapes (
  box
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
