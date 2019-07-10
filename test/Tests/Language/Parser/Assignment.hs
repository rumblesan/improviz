module Tests.Language.Parser.Assignment
  ( parserAssignmentTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion
                                                , assertEqual
                                                )

import qualified Language
import           Language.Ast

parserAssignmentTests :: Test
parserAssignmentTests = testGroup
  "Parser Assignment Tests"
  [ testCase "Parse simple number assignment" test_parse_assign_simple_number
  , testCase "Parse negative number assignment"
             test_parse_assign_negative_number
  , testCase "Parse assignment of an expression" test_parse_expr_assignment
  , testCase "Parse multiple assignments"        test_multiple_assignment
  , testCase "Parse conditional assignments" test_parse_conditional_assignment
  ]

test_parse_assign_simple_number :: Assertion
test_parse_assign_simple_number =
  let program    = "a = 1"
      assignment = AbsoluteAssignment "a" (EVal $ Number 1)
      expected   = Right $ Program [StAssign assignment]
      result     = Language.parse program
  in  assertEqual "" expected result

test_parse_assign_negative_number :: Assertion
test_parse_assign_negative_number =
  let program    = "a = -444"
      assignment = AbsoluteAssignment "a" (UnaryOp "-" (EVal $ Number 444))
      expected   = Right $ Program [StAssign assignment]
      result     = Language.parse program
  in  assertEqual "" expected result

test_parse_expr_assignment :: Assertion
test_parse_expr_assignment =
  let program  = "foo = a + b\n"
      bop = BinaryOp "+" (EVar $ LocalVariable "a") (EVar $ LocalVariable "b")
      expected = Right $ Program [StAssign $ AbsoluteAssignment "foo" bop]
      result   = Language.parse program
  in  assertEqual "" expected result

test_multiple_assignment :: Assertion
test_multiple_assignment =
  let
    program = "foo = 1 + 2\nbar = foo - 2\nbaz = foo * bar\n"
    bop1    = BinaryOp "+" (EVal $ Number 1) (EVal $ Number 2)
    foo     = StAssign $ AbsoluteAssignment "foo" bop1
    bop2    = BinaryOp "-" (EVar $ LocalVariable "foo") (EVal $ Number 2)
    bar     = StAssign $ AbsoluteAssignment "bar" bop2
    bop3 =
      BinaryOp "*" (EVar $ LocalVariable "foo") (EVar $ LocalVariable "bar")
    baz      = StAssign $ AbsoluteAssignment "baz" bop3
    expected = Right $ Program [foo, bar, baz]
    result   = Language.parse program
  in
    assertEqual "" expected result

test_parse_conditional_assignment :: Assertion
test_parse_conditional_assignment =
  let program  = "foo := a + b\n"
      bop = BinaryOp "+" (EVar $ LocalVariable "a") (EVar $ LocalVariable "b")
      expected = Right $ Program [StAssign $ ConditionalAssignment "foo" bop]
      result   = Language.parse program
  in  assertEqual "" expected result
