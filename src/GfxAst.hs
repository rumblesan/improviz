module GfxAst where

type Block = [GfxCommand]

type GfxAst = Block

data Value
  = Variable String
  | Number Double
  deriving (Show, Eq)

data GfxCommand
  = ShapeCommand ShapeGfx (Maybe Block)
  | MatrixCommand MatrixGfx (Maybe Block)
  | ColourCommand ColourGfx (Maybe Block)
  deriving (Show, Eq)

data ShapeGfx
  = Cube Value Value Value
  deriving (Show, Eq)

data MatrixGfx
  = Rotate Value Value Value
--  | Scale Value Value Value
--  | Move Value Value Value
  deriving (Show, Eq)

data ColourGfx
  = Fill Value Value Value
--  | NoFill
--  | Stroke Value Value Value
--  | NoStroke
  deriving (Show, Eq)
