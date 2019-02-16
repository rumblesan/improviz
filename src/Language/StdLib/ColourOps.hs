module Language.StdLib.ColourOps
  ( addColourStdLib
  )
where

import           Control.Monad.Except
import           Data.Maybe                     ( maybe
                                                , listToMaybe
                                                )

import qualified Gfx.Ast                       as GA
import           Language.Ast                   ( Block
                                                , Value(Number, Null, Symbol)
                                                )
import           Language.Interpreter           ( addGfxCommand
                                                , getTextureInfo
                                                , gfxScopedBlock
                                                , setBuiltIn
                                                , setGfxBackground
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )
import           Language.Interpreter.Values    ( getNumberValue )

addColourStdLib :: InterpreterProcess ()
addColourStdLib = do
  setBuiltIn "texture"    texture
  setBuiltIn "frames"     frames
  setBuiltIn "background" background
  setBuiltIn "style"      style

dToC :: Float -> Float
dToC c = max 0 (min c 255) / 255

texture :: [Value] -> Maybe Block -> InterpreterProcess Value
texture args mbBlock = case args of
  Symbol name : rest -> do
    frame <- maybe (return 0) getNumberValue $ listToMaybe rest
    let cmd = GA.ColourCommand $ GA.Fill $ GA.TextureStyle name frame
    case mbBlock of
      Nothing  -> addGfxCommand cmd
      Just blk -> gfxScopedBlock cmd blk
    return Null
  _ -> return Null

frames :: [Value] -> Maybe Block -> InterpreterProcess Value
frames args _ = case args of
  [Symbol name] -> do
    symbolFrames <- getTextureInfo name
    return $ case symbolFrames of
      Nothing -> Null
      Just v  -> Number (fromIntegral v)
  _ -> return Null

background :: [Value] -> Maybe Block -> InterpreterProcess Value
background args _ = do
  (rVal, gVal, bVal) <- case args of
    []                             -> return (255, 255, 255)
    [Number x]                     -> return (x, x, x)
    [Number x, Number y]           -> return (x, y, 0)
    [Number x, Number y, Number z] -> return (x, y, z)
    _ -> throwError "Error with functions to background"
  setGfxBackground (dToC rVal, dToC gVal, dToC bVal)
  return Null


style :: [Value] -> Maybe Block -> InterpreterProcess Value
style styleArgs mbBlock = do
  cmd <- case styleArgs of
    Symbol "fill"     : rest -> fill rest
    Symbol "noFill"   : rest -> noFill
    Symbol "stroke"   : rest -> stroke rest
    Symbol "noStroke" : rest -> noStroke
  case mbBlock of
    Nothing  -> addGfxCommand cmd
    Just blk -> gfxScopedBlock cmd blk
  return Null
 where
  fill :: [Value] -> InterpreterProcess GA.GfxCommand
  fill args = case args of
    [Number r, Number g, Number b, Number a] ->
      return $ GA.ColourCommand $ GA.Fill $ GA.ColourStyle (dToC r)
                                                           (dToC g)
                                                           (dToC b)
                                                           (dToC a)
    _ -> throwError "Error with functions to fill"
  noFill = return $ GA.ColourCommand GA.NoFill
  stroke :: [Value] -> InterpreterProcess GA.GfxCommand
  stroke args = case args of
    [Number r, Number g, Number b, Number a] ->
      return $ GA.ColourCommand $ GA.Stroke (dToC r) (dToC g) (dToC b) (dToC a)
    _ -> throwError "Error with functions to fill"
  noStroke = return $ GA.ColourCommand GA.NoStroke
