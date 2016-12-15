module LCLangLite where

import LCLangLite.LanguageParser (parseProgram)
import LCLangLite.LanguageAst (Block)

parseLCLang :: String -> Maybe Block
parseLCLang program = case parseProgram program of
  Left _ -> Nothing
  Right ast -> Just ast
