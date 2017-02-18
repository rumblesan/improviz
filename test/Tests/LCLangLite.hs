module Tests.LCLangLite (lclangLiteTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Data.Map.Strict
import Control.Monad.State.Strict

import LCLangLite
import LCLangLite.LanguageAst
import LCLangLite.LanguageInterpreter


lclangLiteTests :: Test
lclangLiteTests =
  testGroup "LCLang Lite Tests" [
    testCase "Parsing works as expected" test_simple_parse,
    testCase "Interpreting works as expected" test_simple_interpreter,
    testCase "Interpreting expression works as expected" test_interpret_expression
  ]

test_simple_parse :: Assertion
test_simple_parse =
  let
    program = "cube 1 2 3"
    cube = Application "cube" [EVal $ Number 1, EVal $ Number 2, EVal $ Number 3] Nothing
    expected = Just $ Block [ElExpression $ EApp cube]
  in
    assertEqual "" expected (parseLCLang program)

test_simple_interpreter :: Assertion
test_simple_interpreter =
  let
    block = Block [ElExpression $ EVal $ Number 3]
    result = evalState (interpretLanguage block) emptyState :: Value
    expected = Number 3
  in
    assertEqual "" expected result

test_interpret_expression :: Assertion
test_interpret_expression =
  let
    expr = EVal $ Number 3
    result = evalState (interpretExpression expr) emptyState
    expected = Number 3
  in
    assertEqual "" expected result
