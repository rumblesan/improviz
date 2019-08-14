module Tests.Language.Parser.Loops
  ( parserLoopTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( parserTest )

import           Language.Ast

parserLoopTests :: Test
parserLoopTests = testGroup
  "Loop Tests"
  [ testCase "parses simple loop"        test_parses_simple_loop
  , testCase "parses loop with variable" test_parses_loop_with_var
  , testCase "parses loop with number expression"
             test_parses_loop_with_expr_number
  , testCase "parses loop with number expression and variable"
             test_parses_loop_with_expr_number_and_loop_var
  ]

test_parses_simple_loop :: Assertion
test_parses_simple_loop =
  let program = "4 times\n\trotate()\n\tbox()\n"
      rot =
          ElExpression $ EApp $ Application (LocalVariable "rotate") [] Nothing
      box = ElExpression $ EApp $ Application (LocalVariable "box") [] Nothing
      loop = Loop (EVal $ Number 4) Nothing $ Block [rot, box]
      expected = Program [StLoop loop]
  in  parserTest program expected

test_parses_loop_with_var :: Assertion
test_parses_loop_with_var =
  let
    program = "4 times with i\n\trotate()\n\tbox(i, i, i)\n"
    rot = ElExpression $ EApp $ Application (LocalVariable "rotate") [] Nothing
    boxargs =
      [ ApplicationSingleArg $ EVar $ LocalVariable "i"
      , ApplicationSingleArg $ EVar $ LocalVariable "i"
      , ApplicationSingleArg $ EVar $ LocalVariable "i"
      ]
    box =
      ElExpression $ EApp $ Application (LocalVariable "box") boxargs Nothing
    loop     = Loop (EVal $ Number 4) (Just "i") $ Block [rot, box]
    expected = Program [StLoop loop]
  in
    parserTest program expected

test_parses_loop_with_expr_number :: Assertion
test_parses_loop_with_expr_number =
  let
    program = "(3 + 4) times\n\trotate()\n\tbox()\n"
    numExpr = BinaryOp "+" (EVal $ Number 3) (EVal $ Number 4)
    rot = ElExpression $ EApp $ Application (LocalVariable "rotate") [] Nothing
    box = ElExpression $ EApp $ Application (LocalVariable "box") [] Nothing
    loop = Loop numExpr Nothing $ Block [rot, box]
    expected = Program [StLoop loop]
  in
    parserTest program expected

test_parses_loop_with_expr_number_and_loop_var :: Assertion
test_parses_loop_with_expr_number_and_loop_var =
  let
    program = "(5 * 2) times with i\n\trotate(i)\n\tbox(i)\n"
    numExpr = BinaryOp "*" (EVal $ Number 5) (EVal $ Number 2)
    rotArgs = [ApplicationSingleArg $ EVar $ LocalVariable "i"]
    rot =
      ElExpression $ EApp $ Application (LocalVariable "rotate") rotArgs Nothing
    box = ElExpression $ EApp $ Application
      (LocalVariable "box")
      [ApplicationSingleArg $ EVar $ LocalVariable "i"]
      Nothing
    loop     = Loop numExpr (Just "i") $ Block [rot, box]
    expected = Program [StLoop loop]
  in
    parserTest program expected
