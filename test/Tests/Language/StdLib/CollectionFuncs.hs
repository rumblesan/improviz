module Tests.Language.StdLib.CollectionFuncs
  ( collectionFuncTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import           Data.Maybe                     (fromMaybe)
import qualified Language
import           Language.Ast
import           Language.Interpreter
import qualified Language.Interpreter.Scope     as LS
import           Language.Interpreter.Types

collectionFuncTests :: Test
collectionFuncTests =
  testGroup
    "Collection Function Tests"
    [ testCase "List length function works" test_list_length_func
    , testCase "List elem function works" test_list_length_func
    ]

test_list_elem_func :: Assertion
test_list_elem_func =
  let program = "a = [1,2,3]\nb = elem(a, 2)"
      Right ast = Language.parse program
      startState = Language.initialState 1 []
      finishState = snd $ Language.createGfx startState ast
      result = fromMaybe Null $ LS.getVariable (variables finishState) "b"
      expected = Number 2.0
   in assertEqual "" expected result

test_list_length_func :: Assertion
test_list_length_func =
  let program = "a = [1,2,3]\nb = count(a)"
      Right ast = Language.parse program
      startState = Language.initialState 1 []
      finishState = snd $ Language.createGfx startState ast
      result = fromMaybe Null $ LS.getVariable (variables finishState) "b"
      expected = Number 3.0
   in assertEqual "" expected result
