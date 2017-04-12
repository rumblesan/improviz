module Tests.LanguageParser.Loops (parserLoopTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Language
import Language.LanguageAst
import Language.Interpreter

parserLoopTests :: Test
parserLoopTests =
  testGroup "Parser Loop Tests" [
    testCase "Parse simple loop" test_simple_loop,
    testCase "Parse loop with variable" test_loop_with_var,
    testCase "Parse loop with number expression" test_loop_with_expr_number,
    testCase "Parse loop with number expression and variable" test_loop_with_expr_number_and_loop_var
  ]

test_simple_loop :: Assertion
test_simple_loop =
  let
    program = "4 times {\nrotate();\nbox();\n}\n"
    rot = ElExpression $ EApp $ Application "cube" [] Nothing
    box = ElExpression $ EApp $ Application "box" [] Nothing
    loop = Loop (EVal $ Number 4) Nothing $ Block [rot, box]
    expected = Block [ ElLoop loop ]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_loop_with_var :: Assertion
test_loop_with_var =
  let
    program = "4 times with i {\nrotate();\nbox(i, i, i);\n}\n"
    rot = ElExpression $ EApp $ Application "rotate" [] Nothing
    boxargs = [EVar $ Variable "i", EVar $ Variable "i", EVar $ Variable "i"]
    box = ElExpression $ EApp $ Application "box" boxargs Nothing
    loop = Loop (EVal $ Number 4) (Just "i") $ Block [rot, box]
    expected = Block [ ElLoop loop ]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_loop_with_expr_number :: Assertion
test_loop_with_expr_number =
  let
    program = "(3 + 4) times {\nrotate();\nbox();\n}\n"
    numExpr = BinaryOp "+" (EVal $ Number 3) (EVal $ Number 4)
    rot = ElExpression $ EApp $ Application "rotate" [] Nothing
    box = ElExpression $ EApp $ Application "box" [] Nothing
    loop = Loop numExpr Nothing $ Block [rot, box]
    expected = Block [ ElLoop loop ]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_loop_with_expr_number_and_loop_var :: Assertion
test_loop_with_expr_number_and_loop_var =
  let
    program = "(5 * 2) times with i {\nrotate(i);\nbox();\n}\n"
    numExpr = BinaryOp "*" (EVal $ Number 5) (EVal $ Number 2)
    rotArgs = [EVar $ Variable "i"]
    rot = ElExpression $ EApp $ Application "rotate" rotArgs Nothing
    box = ElExpression $ EApp $ Application "box" [] Nothing
    loop = Loop numExpr Nothing $ Block [rot, box]
    expected = Block [ ElLoop loop ]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

