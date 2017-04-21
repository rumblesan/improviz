module Language.StdLib.BlockHandling (
  handleGfxBlock, runBlock
) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Maybe (maybe, isJust, isNothing)

import Language.Interpreter.Types
import Language.Interpreter (newScope, newGfxScope, interpretBlock, addGfxCommand, getBlock, popBlock)
import Language.Ast

import qualified Gfx.Ast as GA

handleGfxBlock :: (Maybe GA.Block -> GA.GfxCommand) -> Block -> InterpreterProcess ()
handleGfxBlock pc b = do
  newGfxScope
  _ <- interpretBlock b
  s <- get
  let gfxBlock = currentGfx s
  modify (\sm -> sm {currentGfx = head $ gfxStack sm, gfxStack = tail $ gfxStack sm})
  addGfxCommand (pc $ Just gfxBlock)

runBlock :: InterpreterProcess Value
runBlock = newScope(
    do
      -- TODO what should happen to the block attached to runBlock
      callBlock <- popBlock -- Block attached to runBlock call
      prevBlock <- getBlock -- Block passed into upper function
      maybe (return Null) interpretBlock prevBlock
    )

