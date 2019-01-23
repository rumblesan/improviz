module Tests.Language.Parser.Blocks
  ( parserBlocksTests
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

parserBlocksTests :: Test
parserBlocksTests =
  testGroup
    "Parser Blocks Tests"
    [testCase "Parse block with blank lines" test_parse_block_with_blank_lines]

test_parse_block_with_blank_lines :: Assertion
test_parse_block_with_blank_lines =
  let program = "\n\nfill(255, 0, 0)\n\n\trotate()\n\t\n\tbox()\n\n"
      rotate = ElExpression $ EApp $ Application "rotate" [] Nothing
      box = ElExpression $ EApp $ Application "box" [] Nothing
      fill =
        ElExpression $
        EApp $
        Application
          "fill"
          [EVal (Number 255), EVal (Number 0), EVal (Number 0)]
          (Just $ Block [rotate, box])
      expected = Right $ Block [fill]
      result = Language.parse program
   in assertEqual "" expected result
