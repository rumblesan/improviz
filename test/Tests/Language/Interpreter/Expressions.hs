module Tests.Language.Interpreter.Expressions (expressionTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Data.Either
import Data.Maybe
import Data.Map.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import qualified Language
import Language.LanguageAst
import Language.Interpreter


expressionTests :: Test
expressionTests =
  testGroup "Expression Tests" [
    testCase "Number Expression" test_number_expression,
    testCase "Bare Number" test_bare_number
  ]

test_number_expression :: Assertion
test_number_expression =
  let
    block = Block [ElExpression $ EVal $ Number 3]
    result = fst $ Language.interpret [] block
    expected = Right $ Number 3
  in
    assertEqual "" expected result

test_bare_number :: Assertion
test_bare_number =
  let
    expr = EVal $ Number 3
    result = fst $ evalState (runWriterT $ runExceptT $ interpretExpression expr) emptyState
    expected = Right $ Number 3
  in
    assertEqual "" expected result

