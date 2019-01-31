module Tests.Language.Interpreter.Expressions
  ( expressionTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Either
import           Data.Map.Strict
import           Data.Maybe

import qualified Language
import           Language.Ast
import           Language.Interpreter

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
