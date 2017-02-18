module LCLangLite (
  parseLCLang,
  interpretLCLang,
  module LCLangLite.LanguageAst
  ) where

import Control.Monad.State.Strict

import LCLangLite.LanguageParser (parseProgram)
import LCLangLite.LanguageInterpreter (interpretLanguage, emptyState)
import LCLangLite.LanguageAst (Block, Value)

parseLCLang :: String -> Maybe Block
parseLCLang program = case parseProgram program of
  Left _ -> Nothing
  Right ast -> Just ast

interpretLCLang :: Block -> Value
interpretLCLang block =
  evalState (interpretLanguage block) emptyState
