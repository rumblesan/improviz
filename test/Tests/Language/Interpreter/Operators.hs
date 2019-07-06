module Tests.Language.Interpreter.Operators
  ( operatorTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( resultTest )

import           Language.Ast

operatorTests :: Test
operatorTests = testGroup
  "Operator Tests"
  [ testCase "Addition operator"       test_addition_operator
  , testCase "Subtraction operator"    test_subtraction_operator
  , testCase "Multiplication operator" test_multiplication_operator
  , testCase "Division operator"       test_division_operator
  , testCase "Exponent operator"       test_exponent_operator
  , testCase "Modulo operator"         test_modulo_operator
  , testCase "Less Than operator"      test_lessthan_operator
  , testCase "Equal operator"          test_equal_operator
  , testCase "Logical And operator"    test_logical_and_operator
  ]


test_addition_operator :: Assertion
test_addition_operator =
  let program  = "3 + 2"
      expected = Number 5
  in  resultTest program expected "interpreter returns 5"

test_subtraction_operator :: Assertion
test_subtraction_operator =
  let program  = "3 - 2"
      expected = Number 1
  in  resultTest program expected "interpreter returns 1"

test_multiplication_operator :: Assertion
test_multiplication_operator =
  let program  = "3 * 2"
      expected = Number 6
  in  resultTest program expected "interpreter returns 1"

test_division_operator :: Assertion
test_division_operator =
  let program  = "3 / 2"
      expected = Number 1.5
  in  resultTest program expected "interpreter returns 1.5"

test_exponent_operator :: Assertion
test_exponent_operator =
  let program  = "3 ^ 2"
      expected = Number 9
  in  resultTest program expected "interpreter returns 9"

test_modulo_operator :: Assertion
test_modulo_operator =
  let program  = "3 % 2"
      expected = Number 1
  in  resultTest program expected "interpreter returns 1"

test_lessthan_operator :: Assertion
test_lessthan_operator =
  let program  = "3 < 2"
      expected = Number 0
  in  resultTest program expected "interpreter returns 0"

test_equal_operator :: Assertion
test_equal_operator =
  let program  = "2 == 2"
      expected = Number 1
  in  resultTest program expected "interpreter returns 1"

test_logical_and_operator :: Assertion
test_logical_and_operator =
  let program  = "1 && 0"
      expected = Number 0
  in  resultTest program expected "interpreter returns 0"
