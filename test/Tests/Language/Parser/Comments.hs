module Tests.Language.Parser.Comments
  ( parserCommentTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( parserTest )

import           Language.Ast

parserCommentTests :: Test
parserCommentTests = testGroup
  "Comment Tests"
  [ testCase "parses double slash comment" test_parses_double_slash_comment
  , testCase "parses hash comment" test_parses_hash_comment
  ]

test_parses_double_slash_comment :: Assertion
test_parses_double_slash_comment =
  let program    = "//a = 1"
      expected   = Program []
  in  parserTest program expected

test_parses_hash_comment :: Assertion
test_parses_hash_comment =
  let program    = "#a = 1"
      expected   = Program []
  in  parserTest program expected

