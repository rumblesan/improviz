module LCLangLite (
  parseLCLang,
  interpretLCLang,
  createGfx,
  module LCLangLite.LanguageAst
  ) where

import Control.Monad.State.Strict

import LCLangLite.LanguageParser (parseProgram)
import LCLangLite.LanguageInterpreter (interpretLanguage, emptyState, setBuiltIn, currentGfx)
import LCLangLite.LanguageAst (Block, Value)
import qualified LCLangLite.StdLib as SL
import qualified Gfx.GfxAst as GA

parseLCLang :: String -> Maybe Block
parseLCLang program = case parseProgram program of
  Left _ -> Nothing
  Right ast -> Just ast

interpretLCLang :: Block -> Value
interpretLCLang block =
  let
     run = do
       setBuiltIn "box" SL.box ["a", "b", "c"]
       interpretLanguage block
  in
    evalState run emptyState

createGfx :: Block -> GA.Block
createGfx block =
  let
     run = do
       setBuiltIn "box" SL.box ["a", "b", "c"]
       interpretLanguage block
  in
    currentGfx $ execState run emptyState
