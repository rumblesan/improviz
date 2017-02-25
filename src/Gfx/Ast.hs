module Gfx.Ast where

type Block = [GfxCommand]

emptyGfx :: Block
emptyGfx = []

-- TODO
-- This could be more efficient
addGfx :: Block -> GfxCommand -> Block
addGfx block cmd = block ++ [cmd]


type GfxAst = Block

data GfxCommand
  = ShapeCommand ShapeGfx (Maybe Block)
  | MatrixCommand MatrixGfx (Maybe Block)
  | ColourCommand ColourGfx (Maybe Block)
  deriving (Show, Eq)

data ShapeGfx
  = Cube Double Double Double
  | Sphere Double Double Double
  | Cylinder Double Double Double
  | Rectangle Double Double
  | Line Double
  deriving (Show, Eq)

data MatrixGfx
  = Rotate Double Double Double
  | Scale Double Double Double
  | Move Double Double Double
  deriving (Show, Eq)

data ColourGfx
  = Fill Double Double Double Double
  | NoFill
  | Stroke Double Double Double Double
  | NoStroke
  deriving (Show, Eq)
