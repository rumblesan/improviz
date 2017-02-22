module LCLangLite.StdLib where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import LCLangLite.Interpreter.Types
import LCLangLite.Interpreter (newGfxScope, interpretBlock, addGfxCommand, getVariable)
import LCLangLite.Interpreter.Values
import LCLangLite.LanguageAst
import qualified Gfx.GfxAst as GA

noop :: BuiltInFunction
noop _ = return Null

box :: Maybe Block -> InterpreterProcess Value
box block = do
    a <- getVariable "a" >>= getNumberValue
    b <- getVariable "b" >>= getNumberValue
    c <- getVariable "c" >>= getNumberValue
    let partialCmd = GA.ShapeCommand $ GA.Cube a b c
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return $ Number 3
  where
    handleGfxBlock :: (Maybe GA.Block -> GA.GfxCommand) -> Block -> InterpreterProcess ()
    handleGfxBlock pc b = do
      newGfxScope
      _ <- interpretBlock b
      s <- get
      let gfxBlock = currentGfx s
      modify (\sm -> sm {currentGfx = head $ gfxStack sm, gfxStack = tail $ gfxStack sm})
      addGfxCommand (pc $ Just gfxBlock)
