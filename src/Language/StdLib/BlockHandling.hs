module Language.StdLib.BlockHandling
  ( handleGfxBlock
  , addBlockHandlingStdLib
  ) where

import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Maybe                  (isJust, isNothing, maybe)

import           Language.Ast
import           Language.Interpreter        (addGfxCommand, getBlock,
                                              interpretBlock, newGfxScope,
                                              newScope, popBlock, setBuiltIn)
import           Language.Interpreter.Types

import qualified Gfx.Ast                     as GA

addBlockHandlingStdLib :: InterpreterProcess ()
addBlockHandlingStdLib = setBuiltIn "runBlock" runBlock []

handleGfxBlock ::
     (Maybe GA.Block -> GA.GfxCommand) -> Block -> InterpreterProcess ()
handleGfxBlock pc b = do
  newGfxScope
  _ <- interpretBlock b
  s <- get
  let gfxBlock = currentGfx s
  modify
    (\sm -> sm {currentGfx = head $ gfxStack sm, gfxStack = tail $ gfxStack sm})
  addGfxCommand (pc $ Just gfxBlock)

runBlock :: InterpreterProcess Value
runBlock =
  newScope
      -- TODO what should happen to the block attached to runBlock
    (do callBlock <- popBlock -- Block attached to runBlock call
        prevBlock <- getBlock -- Block passed into upper function
        maybe (return Null) interpretBlock prevBlock)
