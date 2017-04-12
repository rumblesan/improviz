module Tests.LanguageParser.Functions (parserFunctionTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Language
import Language.LanguageAst
import Language.Interpreter

parserFunctionTests :: Test
parserFunctionTests =
  testGroup "Parser Function Tests" [
    testCase "Parsing simple function call" test_simple_application,
    testCase "Parsing no arguments function call" test_noargs_application,
    testCase "Parse function with block" test_parse_function_blocks
  ]

test_simple_application :: Assertion
test_simple_application =
  let
    program = "cube(1 2 3);"
    cube = Application "cube" [EVal $ Number 1, EVal $ Number 2, EVal $ Number 3] Nothing
    expected = Block [ElExpression $ EApp cube]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_noargs_application :: Assertion
test_noargs_application =
  let
    program = "foo();"
    cube = Application "foo" [] Nothing
    expected = Block [ElExpression $ EApp cube]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

test_parse_function_blocks :: Assertion
test_parse_function_blocks =
  let
    program = "box(a a 2) {\nb = 2 * 0.5;\nbox(a b 1);\n};\n"
    ass = ElAssign $ Assignment "b" $ BinaryOp "*" (EVal $ Number 2) (EVal $ Number 0.5)
    box2 = ElExpression $ EApp $ Application "box" [
        EVar $ Variable "a",
        EVar $ Variable "b",
        EVal $ Number 1
      ] Nothing
    box1 = ElExpression $ EApp $ Application "box" [
        EVar $ Variable "a",
        EVar $ Variable "a",
        EVal $ Number 2
      ] $ Just (Block [ass, box2])
    expected = Block [box1]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result
