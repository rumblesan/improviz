module Tests.LCLangLite (lclangLiteTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Data.Map.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import qualified Gfx.GfxAst as GA

import LCLangLite
import LCLangLite.LanguageAst
import LCLangLite.LanguageInterpreter


lclangLiteTests :: Test
lclangLiteTests =
  testGroup "LCLang Lite Tests" [
    testCase "Parsing works as expected" test_simple_parse,
    testCase "Parsing assignments works as expected" test_parse_assignment,
    testCase "Interpreting works as expected" test_simple_interpreter,
    testCase "Interpreting expression works as expected" test_interpret_expression,
    testCase "Graphics Creation" test_create_gfx
  ]

test_simple_parse :: Assertion
test_simple_parse =
  let
    program = "cube(1 2 3)"
    cube = Application "cube" [EVal $ Number 1, EVal $ Number 2, EVal $ Number 3] Nothing
    expected = Just $ Block [ElExpression $ EApp cube]
  in
    assertEqual "" expected (parseLCLang program)

test_parse_assignment :: Assertion
test_parse_assignment =
  let
    program = "a = 1"
    assignment = Assignment "a" (EVal $ Number 1)
    expected = Just $ Block [ElAssign assignment]
  in
    assertEqual "" expected (parseLCLang program)

test_simple_interpreter :: Assertion
test_simple_interpreter =
  let
    block = Block [ElExpression $ EVal $ Number 3]
    result = evalState (runWriterT $ interpretLanguage block) emptyState
    logs = []
    expected = (Number 3, logs)
  in
    assertEqual "" expected result

test_interpret_expression :: Assertion
test_interpret_expression =
  let
    expr = EVal $ Number 3
    result = evalState (runWriterT $ interpretExpression expr) emptyState
    logs = []
    expected = (Number 3, logs)
  in
    assertEqual "" expected result

test_create_gfx :: Assertion
test_create_gfx =
  let
    box = EApp $ Application "box" [EVal $ Number 1, EVal $ Number 2, EVal $ Number 1] Nothing
    block = Block [ElExpression box]
    result = createGfx block
    logs = []
    expected = ([GA.ShapeCommand (GA.Cube 1 2 1) Nothing], logs)
  in
    assertEqual "" expected result
