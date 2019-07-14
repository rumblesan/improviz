module Tests.Language.Parser.Operators
  ( parserOperatorTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( parserTest )

import           Language.Ast

parserOperatorTests :: Test
parserOperatorTests = testGroup
  "Operator Tests"
  [testCase "parses complex operator expression" test_parses_complex_operator]

test_parses_complex_operator :: Assertion
test_parses_complex_operator =
  let program = "b = a % 2 == 1 && t < 4"
      modExpr = BinaryOp
        "=="
        (BinaryOp "%" (EVar $ LocalVariable "a") (EVal $ Number 2))
        (EVal $ Number 1)
      ltExpr     = BinaryOp "<" (EVar $ LocalVariable "t") (EVal $ Number 4)
      assignment = AbsoluteAssignment "b" $ BinaryOp "&&" modExpr ltExpr
      expected   = Program [StAssign assignment]
  in  parserTest program expected
