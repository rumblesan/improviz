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
    , testCase
        "Parse function def with named args"
        test_application_with_named_args
    , testCase "Parsing no arguments function call" test_noargs_application
    , testCase "Parse function with block" test_parse_function_blocks
    ]

test_simple_application :: Assertion
test_simple_application =
  let program = "cube(1, 1, i)\n\n"
      cube =
        Application
          "cube"
          [ ApplicationArg Nothing (EVal $ Number 1)
          , ApplicationArg Nothing (EVal $ Number 1)
          , ApplicationArg Nothing (EVar $ Variable "i")
          ]
          Nothing
      expected = Right $ Block [ElExpression $ EApp cube]
      result = Language.parse program
   in assertEqual "" expected result

test_application_list :: Assertion
test_application_list =
  let program = "1, b, 3"
      expected = Right [EVal $ Number 1, EVar $ Variable "b", EVal $ Number 3]
      result = LP.simpleParse (LP.argList LP.expression) program
   in assertEqual "" expected result

test_application_with_var_arg :: Assertion
test_application_with_var_arg =
  let program = "a = 2\ncube(1, a, 3)\n\n"
      assign = ElAssign $ AbsoluteAssignment "a" $ EVal $ Number 2
      cube =
        ElExpression $
        EApp $
        Application
          "cube"
          [ ApplicationArg Nothing (EVal $ Number 1)
          , ApplicationArg Nothing (EVar $ Variable "a")
          , ApplicationArg Nothing (EVal $ Number 3)
          ]
          Nothing
      expected = Right $ Block [assign, cube]
      result = Language.parse program
   in assertEqual "" expected result

test_application_with_named_args :: Assertion
test_application_with_named_args =
  let program = "func foo (a, b = 2) => a - b\nfoo(b = 1, a = 2)"
      func =
        ElFunc $
        Func "foo" [FunctionArg "a" Nothing, FunctionArg "b" (Just $ Number 2)] $
        Block
          [ ElExpression $
            BinaryOp "-" (EVar $ Variable "a") (EVar $ Variable "b")
          ]
      foo =
        ElExpression $
        EApp $
        Application
          "foo"
          [ ApplicationArg (Just "b") (EVal $ Number 1)
          , ApplicationArg (Just "a") (EVal $ Number 2)
          ]
          Nothing
      expected = Right $ Block [func, foo]
      result = Language.parse program
   in assertEqual "" expected result

test_noargs_application :: Assertion
test_noargs_application =
  let program = "foo()"
      cube = Application "foo" [] Nothing
      expected = Right $ Block [ElExpression $ EApp cube]
      result = Language.parse program
   in assertEqual "" expected result

test_parse_function_blocks :: Assertion
test_parse_function_blocks =
  let program = "box(a, a, 2)\n\tb = 2 * 0.5\n\tbox(a, b, 1)\n"
      ass =
        ElAssign $
        AbsoluteAssignment "b" $
        BinaryOp "*" (EVal $ Number 2) (EVal $ Number 0.5)
      box2 =
        ElExpression $
        EApp $
        Application
          "box"
          [ ApplicationArg Nothing (EVar $ Variable "a")
          , ApplicationArg Nothing (EVar $ Variable "b")
          , ApplicationArg Nothing (EVal $ Number 1)
          ]
          Nothing
      box1 =
        ElExpression $
        EApp $
        Application
          "box"
          [ ApplicationArg Nothing (EVar $ Variable "a")
          , ApplicationArg Nothing (EVar $ Variable "a")
          , ApplicationArg Nothing (EVal $ Number 2)
          ] $
        Just (Block [ass, box2])
      expected = Right $ Block [box1]
      result = Language.parse program
   in assertEqual "" expected result
