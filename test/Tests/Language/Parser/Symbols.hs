module Tests.Language.Parser.Symbols
  ( parserSymbolTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import qualified Language
import           Language.Ast

parserSymbolTests :: Test
parserSymbolTests =
  testGroup
    "Parser Symbol Tests"
    [ testCase
        "Parse simple symbol usage as function name"
        test_parse_simple_symbol_usage
    , testCase
        "Parse symbol variable assignment"
        test_parse_symbol_variable_assignment
    ]

test_parse_simple_symbol_usage :: Assertion
test_parse_simple_symbol_usage =
  let program = "texture(:crystal)"
      texture =
        Application
          "texture"
          [ApplicationArg Nothing $ EVal $ Symbol "crystal"]
          Nothing
      expected = Right $ Block [ElExpression $ EApp texture]
      result = Language.parse program
  in assertEqual "" expected result

test_parse_symbol_variable_assignment :: Assertion
test_parse_symbol_variable_assignment =
  let program = "a = :symbol"
      assignment = Assignment "a" (EVal $ Symbol "symbol")
      expected = Right $ Block [ElAssign assignment]
      result = Language.parse program
  in assertEqual "" expected result
