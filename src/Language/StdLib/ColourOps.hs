module Language.StdLib.ColourOps (
  fill, noFill, stroke, noStroke, background
) where


import Control.Monad.Except

import Language.Interpreter.Types
import Language.Interpreter (addGfxCommand, getVarOrNull, interpretBlock, setGfxBackground)
import Language.Interpreter.Values
import Language.LanguageAst
import qualified Gfx.Ast as GA

import Language.StdLib.BlockHandling (handleGfxBlock)

dToC :: Float -> Float
dToC c = max 0 (min c 255) / 255

fill :: Maybe Block -> InterpreterProcess Value
fill block = do
    r <- getVarOrNull "r"
    g <- getVarOrNull "g"
    b <- getVarOrNull "b"
    a <- getVarOrNull "a"
    (rVal, gVal, bVal, aVal) <- case (r, g, b, a) of
      (Null, Null, Null, Null) -> return (255, 255, 255, 255)
      (Number x, Null, Null, Null) -> return (x, x, x, 255)
      (Number x, Number y, Null, Null) -> return (x, x, x, y)
      (Number x, Number y, Number z, Null) -> return (x, y, z, 255)
      (Number x, Number y, Number z, Number w) -> return (x, y, z, w)
      _ -> throwError "Error with functions to fill"
    let partialCmd = GA.ColourCommand $ GA.Fill (dToC rVal) (dToC gVal) (dToC bVal) (dToC aVal)
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

noFill :: Maybe Block -> InterpreterProcess Value
noFill block = do
    let partialCmd = GA.ColourCommand GA.NoFill
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

stroke :: Maybe Block -> InterpreterProcess Value
stroke block = do
    r <- getVarOrNull "r"
    g <- getVarOrNull "g"
    b <- getVarOrNull "b"
    a <- getVarOrNull "a"
    (rVal, gVal, bVal, aVal) <- case (r, g, b, a) of
      (Null, Null, Null, Null) -> return (255, 255, 255, 255)
      (Number x, Null, Null, Null) -> return (x, x, x, 255)
      (Number x, Number y, Null, Null) -> return (x, x, x, y)
      (Number x, Number y, Number z, Null) -> return (x, y, z, 255)
      (Number x, Number y, Number z, Number w) -> return (x, y, z, w)
      _ -> throwError "Error with functions to stroke"
    let partialCmd = GA.ColourCommand $ GA.Stroke (dToC rVal) (dToC gVal) (dToC bVal) (dToC aVal)
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

noStroke :: Maybe Block -> InterpreterProcess Value
noStroke block = do
    let partialCmd = GA.ColourCommand GA.NoStroke
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

background :: Maybe Block -> InterpreterProcess Value
background block = do
    r <- getVarOrNull "r"
    g <- getVarOrNull "g"
    b <- getVarOrNull "b"
    (rVal, gVal, bVal) <- case (r, g, b) of
      (Null, Null, Null) -> return (255, 255, 255)
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, x, x)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to background"
    _ <- setGfxBackground (dToC rVal, dToC gVal, dToC bVal)
    maybe (return Null) interpretBlock block

