module Tests.LanguageParser (lclangParserTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Data.Maybe
import Data.Map.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import qualified Gfx.GfxAst as GA

import LCLangLite
import LCLangLite.LanguageAst
import LCLangLite.Interpreter


lclangParserTests :: Test
lclangParserTests =
  testGroup "LCLang Parser Tests" [
    testCase "Parsing works as expected" test_simple_parse,
    testCase "Parsing assignments works as expected" test_parse_assignment,
    testCase "Parse Assignment" test_parse_expr_assignment,
    testCase "Parse lambda" test_parse_lambda
  ]

test_simple_parse :: Assertion
test_simple_parse =
  let
    program = "cube(1 2 3)"
    cube = Application "cube" [EVal $ Number 1, EVal $ Number 2, EVal $ Number 3] Nothing
    expected = Just $ Block [ElExpression $ EApp cube]
  in
    assertEqual "" expected (parseLCLang program)

test_parse_assignment :: Assertion
test_parse_assignment =
  let
    program = "a = 1"
    assignment = Assignment "a" (EVal $ Number 1)
    expected = Just $ Block [ElAssign assignment]
  in
    assertEqual "" expected (parseLCLang program)

test_parse_expr_assignment :: Assertion
test_parse_expr_assignment =
  let
    program = "foo = a + b"
    bop = BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")
    expected = Just $ Block [ElAssign $ Assignment "foo" $ bop]
    result = parseLCLang program
  in
    assertEqual "" expected result

test_parse_lambda :: Assertion
test_parse_lambda =
  let
    program = "foo = (a b) => a + b"
    block = Block [ElExpression $ BinaryOp "*" (EVar $ Variable "a") (EVar $ Variable "b")]
    lambda = Lambda ["a", "b"] block
    expected = Just $ Block [ElAssign $ Assignment "foo" $ EVal lambda]
    result = parseLCLang program
  in
    assertEqual "" expected result
