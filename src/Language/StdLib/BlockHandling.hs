module Language.StdLib.BlockHandling (
  handleGfxBlock
) where

import Control.Monad.State.Strict

import Language.Interpreter.Types
import Language.Interpreter (newGfxScope, interpretBlock, addGfxCommand)
import Language.LanguageAst

import qualified Gfx.GfxAst as GA

handleGfxBlock :: (Maybe GA.Block -> GA.GfxCommand) -> Block -> InterpreterProcess ()
handleGfxBlock pc b = do
  newGfxScope
  _ <- interpretBlock b
  s <- get
  let gfxBlock = currentGfx s
  modify (\sm -> sm {currentGfx = head $ gfxStack sm, gfxStack = tail $ gfxStack sm})
  addGfxCommand (pc $ Just gfxBlock)
