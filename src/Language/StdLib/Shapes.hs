module Language.StdLib.Shapes
  ( addShapesStdLib
  )
where

import           Control.Monad.Except

import qualified Gfx.Ast                       as GA
import           Language.Ast
import           Language.Interpreter           ( addGfxCommand
                                                , getVarOrNull
                                                , getVariableWithDefault
                                                , gfxScopedBlock
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types
import           Language.Interpreter.Values

addShapesStdLib :: InterpreterProcess ()
addShapesStdLib = do
  setBuiltIn "box" box [VarArg "a", VarArg "b", VarArg "c", BlockArg "block"]
  setBuiltIn "sphere"
             sphere
             [VarArg "a", VarArg "b", VarArg "c", BlockArg "block"]
  setBuiltIn "cylinder"
             cylinder
             [VarArg "a", VarArg "b", VarArg "c", BlockArg "block"]
  setBuiltIn "rectangle" rectangle [VarArg "a", VarArg "b", BlockArg "block"]
  setBuiltIn "line"      line      [VarArg "a", BlockArg "block"]

box :: InterpreterProcess Value
box = do
  a                     <- getVarOrNull "a"
  b                     <- getVarOrNull "b"
  c                     <- getVarOrNull "c"
  blockRef              <- getVarOrNull "block"
  (xSize, ySize, zSize) <- case (a, b, c) of
    (Null    , Null    , Null    ) -> return (1, 1, 1)
    (Number x, Null    , Null    ) -> return (x, x, x)
    (Number x, Number y, Null    ) -> return (x, y, 1)
    (Number x, Number y, Number z) -> return (x, y, z)
    _                              -> throwError "Error with arguments to box"
  let cmd = GA.ShapeCommand $ GA.Cube xSize ySize zSize
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

sphere :: InterpreterProcess Value
sphere = do
  a                     <- getVarOrNull "a"
  b                     <- getVarOrNull "b"
  c                     <- getVarOrNull "c"
  blockRef              <- getVarOrNull "block"
  (xSize, ySize, zSize) <- case (a, b, c) of
    (Null    , Null    , Null    ) -> return (1, 1, 1)
    (Number x, Null    , Null    ) -> return (x, x, x)
    (Number x, Number y, Null    ) -> return (x, y, 1)
    (Number x, Number y, Number z) -> return (x, y, z)
    _ -> throwError "Error with arguments to sphere"
  let cmd = GA.ShapeCommand $ GA.Sphere xSize ySize zSize
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

cylinder :: InterpreterProcess Value
cylinder = do
  a                     <- getVarOrNull "a"
  b                     <- getVarOrNull "b"
  c                     <- getVarOrNull "c"
  blockRef              <- getVarOrNull "block"
  (xSize, ySize, zSize) <- case (a, b, c) of
    (Null    , Null    , Null    ) -> return (1, 1, 1)
    (Number x, Null    , Null    ) -> return (x, x, x)
    (Number x, Number y, Null    ) -> return (x, y, 1)
    (Number x, Number y, Number z) -> return (x, y, z)
    _ -> throwError "Error with arguments to cylinder"
  let cmd = GA.ShapeCommand $ GA.Cylinder xSize ySize zSize
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

rectangle :: InterpreterProcess Value
rectangle = do
  a              <- getVarOrNull "a"
  b              <- getVarOrNull "b"
  blockRef       <- getVarOrNull "block"
  (xSize, ySize) <- case (a, b) of
    (Null    , Null    ) -> return (1, 1)
    (Number x, Null    ) -> return (x, x)
    (Number x, Number y) -> return (x, y)
    _                    -> throwError "Error with arguments to rectangle"
  let cmd = GA.ShapeCommand $ GA.Rectangle xSize ySize
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null

line :: InterpreterProcess Value
line = do
  l        <- getVariableWithDefault "l" (Number 1) >>= getNumberValue
  blockRef <- getVarOrNull "block"
  let cmd = GA.ShapeCommand $ GA.Line l
  case blockRef of
    Null         -> addGfxCommand cmd
    BlockRef blk -> gfxScopedBlock cmd blk
  return Null
