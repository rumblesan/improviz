module Tests.Language.Parser.Lambda (parserLambdaTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Data.Maybe
import Data.Map.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import qualified Gfx.Ast as GA

import qualified Language
import Language.LanguageAst
import Language.Interpreter


parserLambdaTests :: Test
parserLambdaTests =
  testGroup "Parser Lambda Tests" [
    testCase "Parse expression lambda" test_parse_expr_lambda,
    testCase "Parse multiline lambda" test_parse_multiline_lambda
  ]


test_parse_expr_lambda :: Assertion
test_parse_expr_lambda =
  let
    program = "foo = (a, b) => a + b"
    block = Block [ElExpression $ BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")]
    lambda = Lambda ["a", "b"] block
    expected = Right $ Block [ElAssign $ Assignment "foo" $ EVal lambda]
    result = Language.parse program
  in
    assertEqual "" expected result

test_parse_multiline_lambda :: Assertion
test_parse_multiline_lambda =
  let
    program = "foo = (a, b) =>\n\tc = a + b\n\tc * 2\n"
    blockE1 = ElAssign $ Assignment "c" $ BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")
    blockE2 = ElExpression $ BinaryOp "*" (EVar $ Variable "c") (EVal $ Number 2)
    lambda = Lambda ["a", "b"] (Block [blockE1, blockE2])
    expected = Right $ Block [ElAssign $ Assignment "foo" $ EVal lambda]
    result = Language.parse program
  in
    assertEqual "" expected result

