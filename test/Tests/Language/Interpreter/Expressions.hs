module Tests.Language.Interpreter.Expressions
  ( expressionTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import qualified Language
import           Language.Ast

expressionTests :: Test
expressionTests =
  testGroup
    "Expression Tests"
    [testCase "Number Expression" test_number_expression]

test_number_expression :: Assertion
test_number_expression =
  let block = Program [StExpression $ EVal $ Number 3]
      result = Language.interpret [] block
      expected = Right $ Number 3
   in assertEqual "" expected result
