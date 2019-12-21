module Tests.Language.Parser.Functions
  ( parserFunctionTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( parserTest )

import           Language.Ast

parserFunctionTests :: Test
parserFunctionTests = testGroup
  "Function Tests"
  [ testCase "parses simple function call" test_parses_simple_application
  , testCase "parses function call with variable arg"
             test_parses_application_with_var_arg
  , testCase "parses no arguments function call" test_parses_noargs_application
  , testCase "parses function with block"        test_parses_function_blocks
  ]

test_parses_simple_application :: Assertion
test_parses_simple_application =
  let program = "cube(1, 1, i)\n\n"
      cube    = Application
        (LocalVariable "cube")
        [ ApplicationSingleArg $ EVal $ Number 1
        , ApplicationSingleArg $ EVal $ Number 1
        , ApplicationSingleArg $ EVar $ LocalVariable "i"
        ]
        Nothing
      expected = Program [StExpression $ EApp cube]
  in  parserTest program expected

test_parses_application_with_var_arg :: Assertion
test_parses_application_with_var_arg =
  let program = "a = 2\ncube(1, a, 3)\n\n"
      assign  = StAssign $ AbsoluteAssignment "a" $ EVal $ Number 2
      cube    = StExpression $ EApp $ Application
        (LocalVariable "cube")
        [ ApplicationSingleArg $ EVal $ Number 1
        , ApplicationSingleArg $ EVar $ LocalVariable "a"
        , ApplicationSingleArg $ EVal $ Number 3
        ]
        Nothing
      expected = Program [assign, cube]
  in  parserTest program expected

test_parses_noargs_application :: Assertion
test_parses_noargs_application =
  let program  = "foo()"
      cube     = Application (LocalVariable "foo") [] Nothing
      expected = Program [StExpression $ EApp cube]
  in  parserTest program expected

test_parses_function_blocks :: Assertion
test_parses_function_blocks =
  let program = "box(a, a, 2)\n\tb = 2 * 0.5\n\tbox(a, b, 1)\n"
      ass     = ElAssign $ AbsoluteAssignment "b" $ BinaryOp "*"
                                                             (EVal $ Number 2)
                                                             (EVal $ Number 0.5)
      box2 = ElExpression $ EApp $ Application
        (LocalVariable "box")
        [ ApplicationSingleArg $ EVar $ LocalVariable "a"
        , ApplicationSingleArg $ EVar $ LocalVariable "b"
        , ApplicationSingleArg $ EVal $ Number 1
        ]
        Nothing
      box1 =
          StExpression
            $ EApp
            $ Application
                (LocalVariable "box")
                [ ApplicationSingleArg $ EVar $ LocalVariable "a"
                , ApplicationSingleArg $ EVar $ LocalVariable "a"
                , ApplicationSingleArg $ EVal $ Number 2
                ]
            $ Just (Lambda [] Nothing $ Block [ass, box2])
      expected = Program [box1]
  in  parserTest program expected
