module Tests.Language.Parser.Assignment
  ( parserAssignmentTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( parserTest )

import           Language.Ast

parserAssignmentTests :: Test
parserAssignmentTests = testGroup
  "Assignment Tests"
  [ testCase "parses simple number assignment" test_parses_assign_simple_number
  , testCase "parses negative number assignment"
             test_parses_assign_negative_number
  , testCase "parses assignment of an expression" test_parses_expr_assignment
  , testCase "parses multiple assignments" test_parses_multiple_assignment
  , testCase "parses conditional assignments" test_parses_conditional_assignment
  ]

test_parses_assign_simple_number :: Assertion
test_parses_assign_simple_number =
  let program    = "a = 1"
      assignment = AbsoluteAssignment "a" (EVal $ Number 1)
      expected   = Program [StAssign assignment]
  in  parserTest program expected

test_parses_assign_negative_number :: Assertion
test_parses_assign_negative_number =
  let program    = "a = -444"
      assignment = AbsoluteAssignment "a" (UnaryOp "-" (EVal $ Number 444))
      expected   = Program [StAssign assignment]
  in  parserTest program expected

test_parses_expr_assignment :: Assertion
test_parses_expr_assignment =
  let program = "foo = a + b\n"
      bop = BinaryOp "+" (EVar $ LocalVariable "a") (EVar $ LocalVariable "b")
      expected = Program [StAssign $ AbsoluteAssignment "foo" bop]
  in  parserTest program expected

test_parses_multiple_assignment :: Assertion
test_parses_multiple_assignment =
  let
    program = "foo = 1 + 2\nbar = foo - 2\nbaz = foo * bar\n"
    bop1    = BinaryOp "+" (EVal $ Number 1) (EVal $ Number 2)
    foo     = StAssign $ AbsoluteAssignment "foo" bop1
    bop2    = BinaryOp "-" (EVar $ LocalVariable "foo") (EVal $ Number 2)
    bar     = StAssign $ AbsoluteAssignment "bar" bop2
    bop3 =
      BinaryOp "*" (EVar $ LocalVariable "foo") (EVar $ LocalVariable "bar")
    baz      = StAssign $ AbsoluteAssignment "baz" bop3
    expected = Program [foo, bar, baz]
  in
    parserTest program expected

test_parses_conditional_assignment :: Assertion
test_parses_conditional_assignment =
  let program = "foo := a + b\n"
      bop = BinaryOp "+" (EVar $ LocalVariable "a") (EVar $ LocalVariable "b")
      expected = Program [StAssign $ ConditionalAssignment "foo" bop]
  in  parserTest program expected
