module GfxAst where

type Block = [GfxCommand]

type GfxAst = Block

data GfxCommand
  = Shape (Maybe Block)
  | Matrix (Maybe Block)
  | Colour (Maybe Block)
  deriving (Show, Eq)

data Shape
  = Cube Float Float Float
  deriving (Show, Eq)

data Matrix
  = Rotate Float Float Float
--  | Scale Float Float Float
--  | Move Float Float Float
  deriving (Show, Eq)

data Colour
  = Fill Float Float Float
--  | NoFill
--  | Stroke Float Float Float
--  | NoStroke
  deriving (Show, Eq)
