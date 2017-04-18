module Tests.Language.Parser.ErrorCatching (parserErrorCatchingTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Language
import Language.LanguageAst
import Language.Interpreter

parserErrorCatchingTests :: Test
parserErrorCatchingTests =
  testGroup "Parser Error Catching Tests" [
    testCase "Error on open function paren" test_open_function_parens_error
  ]

test_open_function_parens_error :: Assertion
test_open_function_parens_error =
  let
    program = "rotate\n(0.1 0.2\n)\nbox()"
    expected = Left "Error"
    result = Language.parse program
  in
    assertEqual "" expected result

