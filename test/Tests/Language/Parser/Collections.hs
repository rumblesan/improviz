module Tests.Language.Parser.Collections
  ( parserCollectionTests
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

parserCollectionTests :: Test
parserCollectionTests =
  testGroup
    "Collection Tests"
    [ testCase "Parse list collection" test_parse_list
    , testCase "Parse simple list collection" test_parse_simple_list
    ]

test_parse_simple_list :: Assertion
test_parse_simple_list =
  let program = "a = [1, 2, 3]\n"
      list = EVal $ VList [EVal $ Number 1, EVal $ Number 2, EVal $ Number 3]
      aAssign = ElAssign $ AbsoluteAssignment "a" list
      expected = Right $ Block [aAssign]
      result = Language.parse program
   in assertEqual "" expected result

test_parse_list :: Assertion
test_parse_list =
  let program = "a = [1, 2, 3]\nb = elem(a, 2)"
      list = EVal $ VList [EVal $ Number 1, EVal $ Number 2, EVal $ Number 3]
      aAssign = ElAssign $ AbsoluteAssignment "a" list
      bAssign =
        ElAssign $
        AbsoluteAssignment "b" $
        EApp $
        Application
          "elem"
          [ ApplicationArg Nothing $ EVar $ Variable "a"
          , ApplicationArg Nothing $ EVal $ Number 2
          ]
          Nothing
      expected = Right $ Block [aAssign, bAssign]
      result = Language.parse program
   in assertEqual "" expected result
