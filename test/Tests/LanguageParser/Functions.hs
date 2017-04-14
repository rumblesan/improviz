module Tests.LanguageParser.Functions (parserFunctionTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Language
import Language.LanguageAst
import Language.Interpreter
import qualified Language.LanguageParser as LP

parserFunctionTests :: Test
parserFunctionTests =
  testGroup "Parser Function Tests" [
    testCase "Parsing simple function call" test_simple_application,
    testCase "Parsing function application list" test_application_list,
    testCase "Parsing function call with variable arg" test_application_with_var_arg,
    testCase "Parsing no arguments function call" test_noargs_application,
    testCase "Parse function with block" test_parse_function_blocks
  ]

test_simple_application :: Assertion
test_simple_application =
  let
    program = "cube(1, 1, i)\n\n"
    cube = Application "cube" [EVal $ Number 1, EVal $ Number 1, EVar $ Variable "i"] Nothing
    expected = Right $ Block [ElExpression $ EApp cube]
    result = Language.parse program
  in
    assertEqual "" expected result

test_application_list :: Assertion
test_application_list =
  let
    program = "(1, b, 3)"
    expected = Right [EVal $ Number 1, EVar $ Variable "b", EVal $ Number 3]
    result = LP.simpleParse (LP.argList LP.expression) program
  in
    assertEqual "" expected result

test_application_with_var_arg :: Assertion
test_application_with_var_arg =
  let
    program = "a = 2\ncube(1, a, 3)\n\n"
    assign = ElAssign $ Assignment "a" $ EVal $ Number 2
    cube = ElExpression $ EApp $ Application "cube" [EVal $ Number 1, EVar $ Variable "a", EVal $ Number 3] Nothing
    expected = Right $ Block [assign, cube]
    result = Language.parse program
  in
    assertEqual "" expected result

test_noargs_application :: Assertion
test_noargs_application =
  let
    program = "foo()"
    cube = Application "foo" [] Nothing
    expected = Right $ Block [ElExpression $ EApp cube]
    result = Language.parse program
  in
    assertEqual "" expected result

test_parse_function_blocks :: Assertion
test_parse_function_blocks =
  let
    program = "box(a, a, 2)\n\tb = 2 * 0.5\n\tbox(a, b, 1)\n"
    ass = ElAssign $ Assignment "b" $ BinaryOp "*" (EVal $ Number 2) (EVal $ Number 0.5)
    box2 = ElExpression $ EApp $ Application "box" [
        EVar $ Variable "a",
        EVar $ Variable "b",
        EVal $ Number 1
      ] Nothing
    box1 = ElExpression $ EApp $ Application "box" [
        EVar $ Variable "a",
        EVar $ Variable "a",
        EVal $ Number 2
      ] $ Just (Block [ass, box2])
    expected = Right $ Block [box1]
    result = Language.parse program
  in
    assertEqual "" expected result
