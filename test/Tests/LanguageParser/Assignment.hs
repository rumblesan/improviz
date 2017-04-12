module Tests.LanguageParser.Assignment (parserAssignmentTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Language
import Language.LanguageAst

parserAssignmentTests :: Test
parserAssignmentTests =
  testGroup "Parser Assignment Tests" [
    testCase "Parse simple number assignment" test_parse_assign_simple_number,
    testCase "Parse negative number assignment" test_parse_assign_negative_number,
    testCase "Parse assignment of an expression" test_parse_expr_assignment,
    testCase "Parse multiple assignments" test_multiple_assignment
  ]

test_parse_assign_simple_number :: Assertion
test_parse_assign_simple_number =
  let
    program = "a = 1;"
    assignment = Assignment "a" (EVal $ Number 1)
    expected = Block [ElAssign assignment]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_parse_assign_negative_number :: Assertion
test_parse_assign_negative_number =
  let
    program = "a = -444;"
    assignment = Assignment "a" (UnaryOp "-" (EVal $ Number 444))
    expected = Block [ElAssign assignment]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_parse_expr_assignment :: Assertion
test_parse_expr_assignment =
  let
    program = "foo = a + b;"
    bop = BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")
    expected = Block [ElAssign $ Assignment "foo" bop]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_multiple_assignment :: Assertion
test_multiple_assignment =
  let
    program = "foo = 1 + 2;\nbar = foo - 2;\nbaz = foo * bar;\n"
    bop1 = BinaryOp "+" (EVal $ Number 1) (EVal $ Number 2)
    foo = ElAssign $ Assignment "foo" bop1
    bop2 = BinaryOp "-" (EVar $ Variable "foo") (EVal $ Number 2)
    bar = ElAssign $ Assignment "bar" bop2
    bop3 = BinaryOp "*" (EVar $ Variable "foo") (EVar $ Variable "bar")
    baz = ElAssign $ Assignment "baz" bop3
    expected = Block [ foo, bar, baz ]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

