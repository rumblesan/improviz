module Tests.Language.Parser.Lambda
  ( parserLambdaTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Map.Strict
import           Data.Maybe

import qualified Gfx.Ast                        as GA

import qualified Language
import           Language.Ast
import           Language.Interpreter

parserLambdaTests :: Test
parserLambdaTests =
  testGroup
    "Parser Lambda Tests"
    [ testCase "Parse single arg lambda" test_parse_single_arg_lambda
    , testCase "Parse no arg lambda" test_parse_no_arg_lambda
    , testCase "Parse expression lambda" test_parse_expr_lambda
    , testCase "Parse multiline lambda" test_parse_multiline_lambda
    ]

test_parse_no_arg_lambda :: Assertion
test_parse_no_arg_lambda =
  let program = "foo = () => 3 + 1"
      block =
        Block [ElExpression $ BinaryOp "+" (EVal $ Number 3) (EVal $ Number 1)]
      lambda = Lambda [] block
      expected = Right $ Block [ElAssign $ Assignment "foo" $ EVal lambda]
      result = Language.parse program
  in assertEqual "" expected result

test_parse_single_arg_lambda :: Assertion
test_parse_single_arg_lambda =
  let program = "foo = (a) => a + 1"
      block =
        Block
          [ElExpression $ BinaryOp "+" (EVar $ Variable "a") (EVal $ Number 1)]
      lambda = Lambda [FunctionArg "a" Nothing] block
      expected = Right $ Block [ElAssign $ Assignment "foo" $ EVal lambda]
      result = Language.parse program
  in assertEqual "" expected result

test_parse_expr_lambda :: Assertion
test_parse_expr_lambda =
  let program = "foo = (a, b) => a + b"
      block =
        Block
          [ ElExpression $
            BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")
          ]
      lambda = Lambda [FunctionArg "a" Nothing, FunctionArg "b" Nothing] block
      expected = Right $ Block [ElAssign $ Assignment "foo" $ EVal lambda]
      result = Language.parse program
  in assertEqual "" expected result

test_parse_multiline_lambda :: Assertion
test_parse_multiline_lambda =
  let program = "foo = (a, b) =>\n\tc = a + b\n\tc * 2\n"
      blockE1 =
        ElAssign $
        Assignment "c" $
        BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")
      blockE2 =
        ElExpression $ BinaryOp "*" (EVar $ Variable "c") (EVal $ Number 2)
      lambda =
        Lambda
          [FunctionArg "a" Nothing, FunctionArg "b" Nothing]
          (Block [blockE1, blockE2])
      expected = Right $ Block [ElAssign $ Assignment "foo" $ EVal lambda]
      result = Language.parse program
  in assertEqual "" expected result

test_parse_lambda_with_defaults :: Assertion
test_parse_lambda_with_defaults =
  let program = "foo = (a = 1, b = 2) => a + b"
      block =
        Block
          [ ElExpression $
            BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")
          ]
      lambda =
        Lambda
          [FunctionArg "a" $ Just (Number 1), FunctionArg "b" $ Just (Number 2)]
          block
      expected = Right $ Block [ElAssign $ Assignment "foo" $ EVal lambda]
      result = Language.parse program
  in assertEqual "" expected result
