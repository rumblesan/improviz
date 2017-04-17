module Language.StdLib.ColourOps (
  fill, noFill, stroke, noStroke, background
) where


import Language.Interpreter.Types
import Language.Interpreter (addGfxCommand, getVariableWithDefault, interpretBlock, setGfxBackground)
import Language.Interpreter.Values
import Language.LanguageAst
import qualified Gfx.Ast as GA

import Language.StdLib.BlockHandling (handleGfxBlock)

dToC :: Float -> Float
dToC c = max 0 (min c 255) / 255

fill :: Maybe Block -> InterpreterProcess Value
fill block = do
    r <- getVariableWithDefault "r" (Number 255) >>= getNumberValue
    g <- getVariableWithDefault "g" (Number r) >>= getNumberValue
    b <- getVariableWithDefault "b" (Number g) >>= getNumberValue
    a <- getVariableWithDefault "a" (Number 255) >>= getNumberValue
    let partialCmd = GA.ColourCommand $ GA.Fill (dToC r) (dToC g) (dToC b) (dToC a)
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

noFill :: Maybe Block -> InterpreterProcess Value
noFill block = do
    let partialCmd = GA.ColourCommand GA.NoFill
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

stroke :: Maybe Block -> InterpreterProcess Value
stroke block = do
    r <- getVariableWithDefault "r" (Number 255) >>= getNumberValue
    g <- getVariableWithDefault "g" (Number r) >>= getNumberValue
    b <- getVariableWithDefault "b" (Number g) >>= getNumberValue
    a <- getVariableWithDefault "a" (Number 255) >>= getNumberValue
    let partialCmd = GA.ColourCommand $ GA.Stroke (dToC r) (dToC g) (dToC b) (dToC a)
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

noStroke :: Maybe Block -> InterpreterProcess Value
noStroke block = do
    let partialCmd = GA.ColourCommand GA.NoStroke
    maybe (addGfxCommand $ partialCmd Nothing) (handleGfxBlock partialCmd) block
    return Null

background :: Maybe Block -> InterpreterProcess Value
background block =
  do
    r <- getVariableWithDefault "r" (Number 255) >>= getNumberValue
    g <- getVariableWithDefault "g" (Number r) >>= getNumberValue
    b <- getVariableWithDefault "b" (Number g) >>= getNumberValue
    _ <- setGfxBackground (dToC r, dToC g, dToC b)
    maybe (return Null) interpretBlock block
