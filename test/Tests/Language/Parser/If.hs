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
  [ testCase "parses simple if"    test_parses_simple_if
  , testCase "parses if else"      test_parses_if_else
  , testCase "parses if elif else" test_parses_if_elif_else
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

test_parses_if_elif_else :: Assertion
test_parses_if_elif_else =
  let
    program =
      "if (1)\n\tbox()\nelif (2)\n\trectangle()\nelse\n\tball()\nsphere()"
    pred1 = EVal $ Number 1
    pred2 = EVal $ Number 2
    block1 =
      Block [ElExpression $ EApp $ Application (LocalVariable "box") [] Nothing]
    block2 =
      Block
        [ ElExpression $ EApp $ Application (LocalVariable "rectangle")
                                            []
                                            Nothing
        ]
    block3 = Block
      [ElExpression $ EApp $ Application (LocalVariable "ball") [] Nothing]
    afterExpr =
      StExpression $ EApp $ Application (LocalVariable "sphere") [] Nothing
    expected = Program
      [ StIf $ If [(pred1, block1), (pred2, block2), (EVal $ Number 1, block3)]
      , afterExpr
      ]
  in
    parserTest program expected
