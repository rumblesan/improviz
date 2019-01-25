module Tests.Language.Interpreter.Functions
  ( interpreterFunctionTests
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

interpreterFunctionTests :: Test
interpreterFunctionTests =
  testGroup
    "Function Tests"
    [testCase "Test function creation" test_function_creation_and_application]

test_function_creation_and_application :: Assertion
test_function_creation_and_application =
  let block =
        Block
          [ ElExpression $
            BinaryOp "+" (EVar $ LocalVariable "a") (EVal $ Number 1)
          ]
      func = StFunc $ Func "foo" ["a"] block
      appl = StExpression $ EApp $ Application "foo" [EVal $ Number 3] Nothing
      result = fst $ Language.interpret [] $ Program [func, appl]
      expected = Right $ Number 4
   in assertEqual "" expected result
