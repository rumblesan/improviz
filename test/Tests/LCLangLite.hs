module Tests.LCLangLite (lclangLiteTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Data.Map.Strict
import Control.Monad.State.Strict

import LCLangLite
import LCLangLite.LanguageAst


lclangLiteTests :: Test
lclangLiteTests =
  testGroup "LCLang Lite Tests" [
    testCase "Parsing workds as expected" test_simple_parse
  ]

test_simple_parse :: Assertion
test_simple_parse =
  let
    program = "cube 1 2 3"
    cube = Application "cube" [EVal $ Number 1, EVal $ Number 2, EVal $ Number 3] Nothing
    expected = Just $ Block [ElExpression $ EApp cube]
  in
    assertEqual "" expected (parseLCLang program)
