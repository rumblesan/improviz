module Tests.Language.Parser.If
  ( parserIfTests
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

parserIfTests :: Test
parserIfTests =
  testGroup
    "Parser If Tests"
    [ testCase "Parse simple if" test_parse_simple_if
    , testCase "Parse if else" test_parse_if_else
    ]

test_parse_simple_if :: Assertion
test_parse_simple_if =
  let program = "if (1)\n\tbox()"
      pred = EVal $ Number 1
      block =
        Block
          [ElExpression $ EApp $ Application (LocalVariable "box") [] Nothing]
      expected = Right $ Program [StIf $ If pred block Nothing]
      result = Language.parse program
   in assertEqual "" expected result

test_parse_if_else :: Assertion
test_parse_if_else =
  let program = "if (1)\n\tbox()\nelse\n\tball()"
      pred = EVal $ Number 1
      block1 =
        Block
          [ElExpression $ EApp $ Application (LocalVariable "box") [] Nothing]
      block2 =
        Block
          [ElExpression $ EApp $ Application (LocalVariable "ball") [] Nothing]
      expected = Right $ Program [StIf $ If pred block1 (Just block2)]
      result = Language.parse program
   in assertEqual "" expected result
