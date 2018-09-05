module Tests.Language.Parser.Operators
  ( parserOperatorTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import qualified Language
import           Language.Ast

parserOperatorTests :: Test
parserOperatorTests =
  testGroup
    "Parser Operator Tests"
    [testCase "Parse complex operator expression" test_parse_complex_operator]

test_parse_complex_operator :: Assertion
test_parse_complex_operator =
  let program = "b = a % 2 == 1 && t < 4"
      modExpr =
        BinaryOp
          "=="
          (BinaryOp "%" (EVar $ Variable "a") (EVal $ Number 2))
          (EVal $ Number 1)
      ltExpr = BinaryOp "<" (EVar $ Variable "t") (EVal $ Number 4)
      assignment = AbsoluteAssignment "b" $ BinaryOp "&&" modExpr ltExpr
      expected = Right $ Block [ElAssign assignment]
      result = Language.parse program
   in assertEqual "" expected result
