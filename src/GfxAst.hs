module GfxAst where

type Block = [GfxCommand]

type GfxAst = Block

data GfxCommand
  = ShapeCommand ShapeGfx (Maybe Block)
  | MatrixCommand MatrixGfx (Maybe Block)
  | ColourCommand ColourGfx (Maybe Block)
  deriving (Show, Eq)

data ShapeGfx
  = Cube Float Float Float
  deriving (Show, Eq)

data MatrixGfx
  = Rotate Float Float Float
--  | Scale Float Float Float
--  | Move Float Float Float
  deriving (Show, Eq)

data ColourGfx
  = Fill Float Float Float
--  | NoFill
--  | Stroke Float Float Float
--  | NoStroke
  deriving (Show, Eq)
