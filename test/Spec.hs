module Main where

import Data.Monoid (mempty)

import Test.Framework

import Tests.Language (languageTests)
import Tests.LanguageParser (parserTests)
import Tests.LanguageParser.Assignment (parserAssignmentTests)
import Tests.LanguageParser.Functions (parserFunctionTests)
import Tests.LanguageParser.Lambda (parserLambdaTests)
import Tests.LanguageParser.Loops (parserLoopTests)
import Tests.LanguageParser.ErrorCatching (parserErrorCatchingTests)

main :: IO ()
main = defaultMainWithOpts
  [
    languageTests,
    parserTests,
    parserAssignmentTests,
    parserFunctionTests,
    parserLambdaTests,
    parserErrorCatchingTests,
    parserLoopTests
  ]
  mempty
