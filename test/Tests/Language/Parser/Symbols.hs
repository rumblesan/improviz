module Tests.Language.Parser.Symbols
  ( parserSymbolTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( parserTest )

import           Language.Ast

parserSymbolTests :: Test
parserSymbolTests = testGroup
  "Symbol Tests"
  [ testCase "parses simple symbol usage as function name"
             test_parses_simple_symbol_usage
  , testCase "parses symbol variable assignment"
             test_parses_symbol_variable_assignment
  ]

test_parses_simple_symbol_usage :: Assertion
test_parses_simple_symbol_usage =
  let program = "texture(:crystal)"
      texture = Application (LocalVariable "texture")
                            [EVal $ Symbol "crystal"]
                            Nothing
      expected = Program [StExpression $ EApp texture]
  in  parserTest program expected

test_parses_symbol_variable_assignment :: Assertion
test_parses_symbol_variable_assignment =
  let program    = "a = :symbol"
      assignment = AbsoluteAssignment "a" (EVal $ Symbol "symbol")
      expected   = Program [StAssign assignment]
  in  parserTest program expected
