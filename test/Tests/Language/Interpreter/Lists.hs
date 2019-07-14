module Tests.Language.Interpreter.Lists
  ( interpreterListTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( resultTest )
import           Language.Ast                   ( Value(Number) )

interpreterListTests :: Test
interpreterListTests = testGroup
  "List Tests"
  [ testCase "interprets list accessing"         test_list_accessing_program
  , testCase "interprets multiple list accesses" test_multiple_list_accessing
  ]

test_list_accessing_program :: Assertion
test_list_accessing_program =
  let program  = "a = [1+2, 3+4, 5+6]\n\
        \b = -1\n\
        \a[b + 1]"
      expected = Number 3
  in  resultTest program expected "returns 3"

test_multiple_list_accessing :: Assertion
test_multiple_list_accessing =
  let program  = "[1, [[1, 2], [3, 4]]][1][0][1]"
      expected = Number 2
  in  resultTest program expected "returns 2"
