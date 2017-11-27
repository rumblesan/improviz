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
  = ShapeCommand ShapeGfx
                 (Maybe Block)
  | MatrixCommand MatrixGfx
                  (Maybe Block)
  | ColourCommand ColourGfx
                  (Maybe Block)
  deriving (Show, Eq)

data ShapeGfx
  = Cube Float
         Float
         Float
  | Sphere Float
           Float
           Float
  | Cylinder Float
             Float
             Float
  | Rectangle Float
              Float
  | Line Float
  deriving (Show, Eq)

data MatrixGfx
  = Rotate Float
           Float
           Float
  | Scale Float
          Float
          Float
  | Move Float
         Float
         Float
  deriving (Show, Eq)

data ColourGfx
  = Fill StyleGfx
  | NoFill
  | Stroke Float
           Float
           Float
           Float
  | NoStroke
  deriving (Show, Eq)

data StyleGfx
  = ColourStyle Float
                Float
                Float
                Float
  | TextureStyle String
                 Float
  deriving (Show, Eq)
