module Tests.Language.Parser.ErrorCatching
  ( parserErrorCatchingTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertBool)

import           Data.Either                    (isLeft)

import qualified Language

parserErrorCatchingTests :: Test
parserErrorCatchingTests =
  testGroup
    "Parser Error Catching Tests"
    [ testCase "Error on open function paren" test_open_function_parens_error
    , testCase "Error on remaining program" test_remaining_program_error
    ]

test_open_function_parens_error :: Assertion
test_open_function_parens_error =
  let program = "rotate\n(0.1 0.2\n)\nbox()"
      result = Language.parse program
   in assertBool "" (isLeft result)

test_remaining_program_error :: Assertion
test_remaining_program_error =
  let program = "box()\n("
      result = Language.parse program
   in assertBool "Should fail due to hanging paren" (isLeft result)
