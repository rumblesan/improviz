module Tests.Language.Interpreter.Functions
  ( interpreterFunctionTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( gfxTest
                                                , resultTest
                                                )
import qualified TestHelpers.GfxAst            as GA

import           Language.Ast                   ( Value(Number) )

interpreterFunctionTests :: Test
interpreterFunctionTests = testGroup
  "Function Tests"
  [ testCase "interprets function creation"
             test_function_creation_and_application
  , testCase "interprets function as argument" test_function_as_arg
  , testCase "interprets spread arguments"     test_spread_argument
  ]

test_function_creation_and_application :: Assertion
test_function_creation_and_application =
  let program  = "func foo (a) => a + 1\n\
         \foo(3)\n"
      expected = Number 4
  in  resultTest program expected "interpreter returns 4"

test_function_as_arg :: Assertion
test_function_as_arg =
  let
    program =
      "a = 2\nb = 3\nfunc foo (c, d) => c * d\nshape(:cube, b, a, foo(a, b))\n"
    expectedGfx = [GA.ShapeCommand (GA.ShapeGfx "cube" 3 2 6)]
  in
    gfxTest program expectedGfx

test_spread_argument :: Assertion
test_spread_argument =
  let
    program
      = "list = [1, 2, 3]\n\
         \func foo (a, b, c) => a + b + c\n\
         \foo(...list)\n"
    expected = Number 6
  in
    resultTest program expected "interpreter returns 6"
