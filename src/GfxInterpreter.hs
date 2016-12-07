module GfxInterpreter where

import Data.Maybe (maybe)

import GfxAst


type GfxOutput = String

interpretGfx :: GfxAst -> [GfxOutput]
interpretGfx = interpretBlock

interpretBlock :: Block -> [GfxOutput]
interpretBlock block = block >>= interpretCommand

interpretCommand :: GfxCommand -> [GfxOutput]
interpretCommand (ShapeCommand shape block) =
  let
    blockMap :: GfxAst -> [GfxOutput]
    blockMap = newScope (show shape)
  in maybe (interpretShape shape) blockMap block
interpretCommand (MatrixCommand matrix block) =
  let
    blockMap :: GfxAst -> [GfxOutput]
    blockMap = newScope (show matrix)
  in maybe (interpretMatrix matrix) blockMap block
interpretCommand (ColourCommand colour block) =
  let
    blockMap :: GfxAst -> [GfxOutput]
    blockMap = newScope (show colour)
  in maybe (interpretColour colour) blockMap block


interpretShape :: ShapeGfx -> [GfxOutput]
interpretShape shape = [show shape]

interpretMatrix :: MatrixGfx -> [GfxOutput]
interpretMatrix matrix = [show matrix]

interpretColour :: ColourGfx -> [GfxOutput]
interpretColour colour = [show colour]


newScope :: String -> GfxAst -> [GfxOutput]
newScope name block =
  [name ++ " scope enter"] ++ interpretBlock block ++ [name ++ " scope leave"]
