module Main where

import Data.Monoid (mempty)

import Test.Framework

import Tests.Language (languageTests)
import Tests.Language.Interpreter.Operators (operatorTests)
import Tests.Language.Parser (parserTests)
import Tests.Language.Parser.Assignment (parserAssignmentTests)
import Tests.Language.Parser.Functions (parserFunctionTests)
import Tests.Language.Parser.Lambda (parserLambdaTests)
import Tests.Language.Parser.Loops (parserLoopTests)
import Tests.Language.Parser.ErrorCatching (parserErrorCatchingTests)

main :: IO ()
main = defaultMainWithOpts
  [
    languageTests,
    operatorTests,
    parserTests,
    parserAssignmentTests,
    parserFunctionTests,
    parserLambdaTests,
    parserErrorCatchingTests,
    parserLoopTests
  ]
  mempty
