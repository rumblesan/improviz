module Tests.Language.Parser.Blocks
  ( parserBlocksTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import qualified Language
import           Language.Ast

parserBlocksTests :: Test
parserBlocksTests =
  testGroup
    "Parser Blocks Tests"
    [testCase "Parse block with blank lines" test_parse_block_with_blank_lines]

test_parse_block_with_blank_lines :: Assertion
test_parse_block_with_blank_lines =
  let program = "\n\nfill(255, 0, 0)\n\n\trotate()\n\t\n\tbox()\n\n"
      rotate =
        ElExpression $ EApp $ Application (LocalVariable "rotate") [] Nothing
      box = ElExpression $ EApp $ Application (LocalVariable "box") [] Nothing
      fill =
        StExpression $
        EApp $
        Application
          (LocalVariable "fill")
          [EVal (Number 255), EVal (Number 0), EVal (Number 0)]
          (Just $ Block [rotate, box])
      expected = Right $ Program [fill]
      result = Language.parse program
   in assertEqual "" expected result
