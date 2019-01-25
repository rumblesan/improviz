module Tests.Language.Parser.Functions
  ( parserFunctionTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import qualified Language
import           Language.Ast
import           Language.Interpreter
import qualified Language.Parser                as LP

parserFunctionTests :: Test
parserFunctionTests =
  testGroup
    "Parser Function Tests"
    [ testCase "Parsing simple function call" test_simple_application
    , testCase "Parsing function application list" test_application_list
    , testCase
        "Parsing function call with variable arg"
        test_application_with_var_arg
    , testCase "Parsing no arguments function call" test_noargs_application
    , testCase "Parse function with block" test_parse_function_blocks
    ]

test_simple_application :: Assertion
test_simple_application =
  let program = "cube(1, 1, i)\n\n"
      cube =
        Application
          "cube"
          [EVal $ Number 1, EVal $ Number 1, EVar $ LocalVariable "i"]
          Nothing
      expected = Right $ Program [StExpression $ EApp cube]
      result = Language.parse program
   in assertEqual "" expected result

test_application_list :: Assertion
test_application_list =
  let program = "1, b, 3"
      expected =
        Right [EVal $ Number 1, EVar $ LocalVariable "b", EVal $ Number 3]
      result = LP.simpleParse (LP.argList LP.expression) program
   in assertEqual "" expected result

test_application_with_var_arg :: Assertion
test_application_with_var_arg =
  let program = "var a = 2\ncube(1, a, 3)\n\n"
      assign = StAssign $ AbsoluteAssignment "a" $ EVal $ Number 2
      cube =
        StExpression $
        EApp $
        Application
          "cube"
          [EVal $ Number 1, EVar $ LocalVariable "a", EVal $ Number 3]
          Nothing
      expected = Right $ Program [assign, cube]
      result = Language.parse program
   in assertEqual "" expected result

test_noargs_application :: Assertion
test_noargs_application =
  let program = "foo()"
      cube = Application "foo" [] Nothing
      expected = Right $ Program [StExpression $ EApp cube]
      result = Language.parse program
   in assertEqual "" expected result

test_parse_function_blocks :: Assertion
test_parse_function_blocks =
  let program = "box(a, a, 2)\n\tvar b = 2 * 0.5\n\tbox(a, b, 1)\n"
      ass =
        ElAssign $
        AbsoluteAssignment "b" $
        BinaryOp "*" (EVal $ Number 2) (EVal $ Number 0.5)
      box2 =
        ElExpression $
        EApp $
        Application
          "box"
          [EVar $ LocalVariable "a", EVar $ LocalVariable "b", EVal $ Number 1]
          Nothing
      box1 =
        StExpression $
        EApp $
        Application
          "box"
          [EVar $ LocalVariable "a", EVar $ LocalVariable "a", EVal $ Number 2] $
        Just (Block [ass, box2])
      expected = Right $ Program [box1]
      result = Language.parse program
   in assertEqual "" expected result
