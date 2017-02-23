module Language.StdLib.Shapes (
  box
) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Language.Interpreter.Types
import Language.Interpreter (addGfxCommand, getVariable)
import Language.Interpreter.Values
import Language.LanguageAst
import qualified Gfx.GfxAst as GA

import Language.StdLib.BlockHandling (handleGfxBlock)


box :: Maybe Block -> InterpreterProcess Value
box block = do
    a <- getVariable "a" >>= getNumberValue
    b <- getVariable "b" >>= getNumberValue
    c <- getVariable "c" >>= getNumberValue
    let partialCmd = GA.ShapeCommand $ GA.Cube a b c
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null
