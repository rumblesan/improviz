module Tests.Language.Interpreter.Functions
  ( interpreterFunctionTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( resultTest )

import           Language.Ast

interpreterFunctionTests :: Test
interpreterFunctionTests = testGroup
  "Function Tests"
  [testCase "Test function creation" test_function_creation_and_application]

test_function_creation_and_application :: Assertion
test_function_creation_and_application =
  let program  = "func foo (a) => a + 1\n\
         \foo(3)\n"
      expected = Number 4
  in  resultTest program expected "interpreter returns 4"
