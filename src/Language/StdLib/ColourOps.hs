module Language.StdLib.ColourOps
  ( addColourStdLib
  )
where

import           Control.Monad.Except
import           Data.Map.Strict               as M

import qualified Gfx.Ast                       as GA
import qualified Gfx.EngineState               as GE
import           Language.Ast
import           Language.Interpreter           ( addGfxCommand
                                                , getEngineInfo
                                                , getVarOrNull
                                                , getVariable
                                                , getVariableWithDefault
                                                , gfxScopedBlock
                                                , setBuiltIn
                                                , setGfxBackground
                                                )
import           Language.Interpreter.Types
import           Language.Interpreter.Values

addColourStdLib :: InterpreterProcess ()
addColourStdLib = do
  setBuiltIn
    "fill"
    fill
    [VarArg "r", VarArg "g", VarArg "b", VarArg "a", BlockArg "block"]
  setBuiltIn "texture" texture [VarArg "name", VarArg "frame", BlockArg "block"]
  setBuiltIn "frames"  frames  [VarArg "name"]
  setBuiltIn "noFill"  noFill  [BlockArg "block"]
  setBuiltIn
    "stroke"
    stroke
    [VarArg "r", VarArg "g", VarArg "b", VarArg "a", BlockArg "block"]
  setBuiltIn "noStroke"   noStroke   [BlockArg "block"]
  setBuiltIn "background" background [VarArg "r", VarArg "g", VarArg "b"]

dToC :: Float -> Float
dToC c = max 0 (min c 255) / 255

fill :: InterpreterProcess Value
fill = do
  r                        <- getVarOrNull "r"
  g                        <- getVarOrNull "g"
  b                        <- getVarOrNull "b"
  a                        <- getVarOrNull "a"
  blockRef                 <- getVarOrNull "block"
  (rVal, gVal, bVal, aVal) <- case (r, g, b, a) of
    (Null, Null, Null, Null) -> return (255, 255, 255, 255)
    (Number x, Null, Null, Null) -> return (x, x, x, 255)
    (Number x, Number y, Null, Null) -> return (x, x, x, y)
    (Number x, Number y, Number z, Null) -> return (x, y, z, 255)
    (Number x, Number y, Number z, Number w) -> return (x, y, z, w)
    _ -> throwError "Error with functions to fill"
  let cmd = GA.ColourCommand $ GA.Fill $ GA.ColourStyle (dToC rVal)
                                                        (dToC gVal)
                                                        (dToC bVal)
                                                        (dToC aVal)
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

texture :: InterpreterProcess Value
texture = do
  textName <- getVariable "name"
  frame    <- getVariableWithDefault "frame" (Number 0) >>= getNumberValue
  blockRef <- getVarOrNull "block"
  case textName of
    Symbol name -> do
      let cmd = GA.ColourCommand $ GA.Fill $ GA.TextureStyle name frame
      case blockRef of
        Null         -> addGfxCommand cmd
        BlockRef blk -> gfxScopedBlock cmd blk
      return Null
    _ -> return Null

frames :: InterpreterProcess Value
frames = do
  ei       <- getEngineInfo
  textName <- getVariable "name"
  return $ case textName of
    Symbol name -> case M.lookup name (GE.textureFrames ei) of
      Nothing -> Null
      Just v  -> Number (fromIntegral v)
    _ -> Null

noFill :: InterpreterProcess Value
noFill = do
  blockRef <- getVarOrNull "block"
  let cmd = GA.ColourCommand GA.NoFill
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

stroke :: InterpreterProcess Value
stroke = do
  r                        <- getVarOrNull "r"
  g                        <- getVarOrNull "g"
  b                        <- getVarOrNull "b"
  a                        <- getVarOrNull "a"
  blockRef                 <- getVarOrNull "block"
  (rVal, gVal, bVal, aVal) <- case (r, g, b, a) of
    (Null, Null, Null, Null) -> return (255, 255, 255, 255)
    (Number x, Null, Null, Null) -> return (x, x, x, 255)
    (Number x, Number y, Null, Null) -> return (x, x, x, y)
    (Number x, Number y, Number z, Null) -> return (x, y, z, 255)
    (Number x, Number y, Number z, Number w) -> return (x, y, z, w)
    _ -> throwError "Error with functions to stroke"
  let cmd = GA.ColourCommand
        $ GA.Stroke (dToC rVal) (dToC gVal) (dToC bVal) (dToC aVal)
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

noStroke :: InterpreterProcess Value
noStroke = do
  blockRef <- getVarOrNull "block"
  let cmd = GA.ColourCommand GA.NoStroke
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

background :: InterpreterProcess Value
background = do
  r                  <- getVarOrNull "r"
  g                  <- getVarOrNull "g"
  b                  <- getVarOrNull "b"
  (rVal, gVal, bVal) <- case (r, g, b) of
    (Null    , Null    , Null    ) -> return (255, 255, 255)
    (Number x, Null    , Null    ) -> return (x, x, x)
    (Number x, Number y, Null    ) -> return (x, x, x)
    (Number x, Number y, Number z) -> return (x, y, z)
    _ -> throwError "Error with functions to background"
  _ <- setGfxBackground (dToC rVal, dToC gVal, dToC bVal)
  return Null
