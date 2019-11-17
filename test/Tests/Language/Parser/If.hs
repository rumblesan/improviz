module Tests.Language.Parser.If
  ( parserIfTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( parserTest )

import           Language.Ast

parserIfTests :: Test
parserIfTests = testGroup
  "If Tests"
  [ testCase "parses simple if" test_parses_simple_if
  , testCase "parses if else"   test_parses_if_else
  ]

test_parses_simple_if :: Assertion
test_parses_simple_if =
  let
    program = "if (1)\n\tbox()"
    pred    = EVal $ Number 1
    block =
      Block [ElExpression $ EApp $ Application (LocalVariable "box") [] Nothing]
    expected = Program [StIf $ If [(pred, block)]]
  in
    parserTest program expected

test_parses_if_else :: Assertion
test_parses_if_else =
  let
    program = "if (1)\n\tbox()\nelse\n\tball()"
    pred    = EVal $ Number 1
    block1 =
      Block [ElExpression $ EApp $ Application (LocalVariable "box") [] Nothing]
    block2 = Block
      [ElExpression $ EApp $ Application (LocalVariable "ball") [] Nothing]
    expected = Program [StIf $ If [(pred, block1), (EVal $ Number 1, block2)]]
  in
    parserTest program expected
