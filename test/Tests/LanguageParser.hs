module Tests.LanguageParser (parserTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Data.Maybe
import Data.Map.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import qualified Gfx.Ast as GA

import qualified Language
import Language.LanguageAst
import Language.Interpreter


parserTests :: Test
parserTests =
  testGroup "Parser Tests" [
    testCase "Parsing works as expected" test_simple_parse,
    testCase "Parsing assignments works as expected" test_parse_assignment,
    testCase "Parse Assignment" test_parse_expr_assignment,
    testCase "Parse expression lambda" test_parse_expr_lambda,
    testCase "Parse multiline lambda" test_parse_multiline_lambda,
    testCase "Parse function with block" test_parse_function_blocks
  ]

test_simple_parse :: Assertion
test_simple_parse =
  let
    program = "cube(1 2 3);"
    cube = Application "cube" [EVal $ Number 1, EVal $ Number 2, EVal $ Number 3] Nothing
    expected = Block [ElExpression $ EApp cube]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_parse_assignment :: Assertion
test_parse_assignment =
  let
    program = "a = 1;"
    assignment = Assignment "a" (EVal $ Number 1)
    expected = Block [ElAssign assignment]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_parse_expr_assignment :: Assertion
test_parse_expr_assignment =
  let
    program = "foo = a + b;"
    bop = BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")
    expected = Block [ElAssign $ Assignment "foo" $ bop]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_parse_expr_lambda :: Assertion
test_parse_expr_lambda =
  let
    program = "foo = (a b) => a + b;"
    block = Block [ElExpression $ BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")]
    lambda = Lambda ["a", "b"] block
    expected = Block [ElAssign $ Assignment "foo" $ EVal lambda]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_parse_multiline_lambda :: Assertion
test_parse_multiline_lambda =
  let
    program = "foo = (a b) => {\nc = a + b;\nc * 2;\n};"
    blockE1 = ElAssign $ Assignment "c" $ BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")
    blockE2 = ElExpression $ BinaryOp "*" (EVar $ Variable "c") (EVal $ Number 2)
    lambda = Lambda ["a", "b"] (Block [blockE1, blockE2])
    expected = Block [ElAssign $ Assignment "foo" $ EVal lambda]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_parse_function_blocks :: Assertion
test_parse_function_blocks =
  let
    program = "a = 2;\nbox(a a 2) {\nb = 2 * 0.5;\nbox(a b 1);\n};\n"
    ass1 = ElAssign $ Assignment "a" $ EVal $ Number 2
    ass2 = ElAssign $ Assignment "b" $ BinaryOp "*" (EVal $ Number 2) (EVal $ Number 0.5)
    box2 = ElExpression $ EApp $ Application "box" [
        (EVar $ Variable "a"),
        (EVar $ Variable "b"),
        (EVal $ Number 1)
      ] Nothing
    box1 = ElExpression $ EApp $ Application "box" [
        (EVar $ Variable "a"),
        (EVar $ Variable "a"),
        (EVal $ Number 2)
      ] $ Just (Block [ass2, box2])
    expected = Block [ass1, box1]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result
