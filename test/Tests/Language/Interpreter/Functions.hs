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
  [ testCase "Test function creation"    test_function_creation_and_application
  , testCase "Test function as argument" test_function_as_arg
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
    expectedGfx = [GA.ShapeCommand (GA.Cube 3 2 6)]
  in
    gfxTest program expectedGfx
