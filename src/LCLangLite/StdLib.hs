module LCLangLite.StdLib where

import Control.Monad.State.Strict

import LCLangLite.LanguageInterpreter (InterpreterProcess, newGfxScope, interpretBlock, addGfxCommand, currentGfx, gfxStack, getVariable)
import LCLangLite.LanguageAst
import qualified Gfx.GfxAst as GA

box :: (Monad m) => Maybe Block -> InterpreterProcess m Value
box block = do
    a <- getVariable "a"
    b <- getVariable "b"
    c <- getVariable "c"
    let partialCmd = GA.ShapeCommand $ GA.Cube 1 2 1
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null
  where
    handleGfxBlock :: (Monad m) => (Maybe GA.Block -> GA.GfxCommand) -> Block -> InterpreterProcess m ()
    handleGfxBlock pc b = do
      newGfxScope
      _ <- interpretBlock b
      s <- get
      let gfxBlock = currentGfx s
      modify (\sm -> sm {currentGfx = head $ gfxStack sm, gfxStack = tail $ gfxStack sm})
      addGfxCommand (pc $ Just gfxBlock)
