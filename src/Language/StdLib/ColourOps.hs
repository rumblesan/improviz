module Language.StdLib.ColourOps
  ( addColourStdLib
  ) where

import           Control.Monad.Except
import           Data.Map.Strict             as M

import qualified Gfx.Ast                     as GA
import qualified Gfx.EngineState             as GE
import           Language.Ast
import           Language.Interpreter        (addGfxCommand, getBlock,
                                              getEngineInfo, getVarOrNull,
                                              getVariable,
                                              getVariableWithDefault,
                                              gfxScopedBlock, interpretBlock,
                                              setBuiltIn, setGfxBackground)
import           Language.Interpreter.Types
import           Language.Interpreter.Values

addColourStdLib :: InterpreterProcess ()
addColourStdLib = do
  setBuiltIn "fill" fill ["r", "g", "b", "a"]
  setBuiltIn "texture" texture ["name", "frame"]
  setBuiltIn "frames" frames ["name"]
  setBuiltIn "noFill" noFill []
  setBuiltIn "stroke" stroke ["r", "g", "b", "a"]
  setBuiltIn "noStroke" noStroke []
  setBuiltIn "background" background ["r", "g", "b"]

dToC :: Float -> Float
dToC c = max 0 (min c 255) / 255

fill :: InterpreterProcess Value
fill = do
  r <- getVarOrNull "r"
  g <- getVarOrNull "g"
  b <- getVarOrNull "b"
  a <- getVarOrNull "a"
  (rVal, gVal, bVal, aVal) <-
    case (r, g, b, a) of
      (Null, Null, Null, Null) -> return (255, 255, 255, 255)
      (Number x, Null, Null, Null) -> return (x, x, x, 255)
      (Number x, Number y, Null, Null) -> return (x, x, x, y)
      (Number x, Number y, Number z, Null) -> return (x, y, z, 255)
      (Number x, Number y, Number z, Number w) -> return (x, y, z, w)
      _ -> throwError "Error with functions to fill"
  let cmd =
        GA.ColourCommand $
        GA.Fill $ GA.ColourStyle (dToC rVal) (dToC gVal) (dToC bVal) (dToC aVal)
  block <- getBlock
  maybe (addGfxCommand cmd) (gfxScopedBlock cmd) block
  return Null

texture :: InterpreterProcess Value
texture = do
  textName <- getVariable "name"
  frame <- getVariableWithDefault "frame" (Number 0) >>= getNumberValue
  case textName of
    Symbol name -> do
      let cmd = GA.ColourCommand $ GA.Fill $ GA.TextureStyle name frame
      block <- getBlock
      maybe (addGfxCommand cmd) (gfxScopedBlock cmd) block
      return Null
    _ -> return Null

frames :: InterpreterProcess Value
frames = do
  ei <- getEngineInfo
  textName <- getVariable "name"
  return $
    case textName of
      Symbol name ->
        case M.lookup name (GE.textureFrames ei) of
          Nothing -> Null
          Just v  -> Number (fromIntegral v)
      _ -> Null

noFill :: InterpreterProcess Value
noFill = do
  let cmd = GA.ColourCommand GA.NoFill
  block <- getBlock
  maybe (addGfxCommand cmd) (gfxScopedBlock cmd) block
  return Null

stroke :: InterpreterProcess Value
stroke = do
  r <- getVarOrNull "r"
  g <- getVarOrNull "g"
  b <- getVarOrNull "b"
  a <- getVarOrNull "a"
  (rVal, gVal, bVal, aVal) <-
    case (r, g, b, a) of
      (Null, Null, Null, Null) -> return (255, 255, 255, 255)
      (Number x, Null, Null, Null) -> return (x, x, x, 255)
      (Number x, Number y, Null, Null) -> return (x, x, x, y)
      (Number x, Number y, Number z, Null) -> return (x, y, z, 255)
      (Number x, Number y, Number z, Number w) -> return (x, y, z, w)
      _ -> throwError "Error with functions to stroke"
  let cmd =
        GA.ColourCommand $
        GA.Stroke (dToC rVal) (dToC gVal) (dToC bVal) (dToC aVal)
  block <- getBlock
  maybe (addGfxCommand cmd) (gfxScopedBlock cmd) block
  return Null

noStroke :: InterpreterProcess Value
noStroke = do
  let cmd = GA.ColourCommand GA.NoStroke
  block <- getBlock
  maybe (addGfxCommand cmd) (gfxScopedBlock cmd) block
  return Null

background :: InterpreterProcess Value
background = do
  r <- getVarOrNull "r"
  g <- getVarOrNull "g"
  b <- getVarOrNull "b"
  (rVal, gVal, bVal) <-
    case (r, g, b) of
      (Null, Null, Null) -> return (255, 255, 255)
      (Number x, Null, Null) -> return (x, x, x)
      (Number x, Number y, Null) -> return (x, x, x)
      (Number x, Number y, Number z) -> return (x, y, z)
      _ -> throwError "Error with functions to background"
  _ <- setGfxBackground (dToC rVal, dToC gVal, dToC bVal)
  block <- getBlock
  maybe (return Null) interpretBlock block
