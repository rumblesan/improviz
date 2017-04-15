module Tests.Language (languageTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Data.Either
import Data.Maybe
import Data.Map.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import qualified Gfx.Ast as GA
import Gfx (Scene(..))

import qualified Language
import Language.LanguageAst
import Language.Interpreter


languageTests :: Test
languageTests =
  testGroup "Language Tests" [
    testCase "Interpreting works as expected" test_simple_interpreter,
    testCase "Interpreting expression works as expected" test_interpret_expression,
    testCase "Graphics Creation" test_create_gfx,
    testCase "Basic program" test_basic_program,
    testCase "Loop program" test_loop_program
  ]

test_simple_interpreter :: Assertion
test_simple_interpreter =
  let
    block = Block [ElExpression $ EVal $ Number 3]
    result = evalState (runWriterT $ runExceptT $ interpretLanguage block) emptyState
    logs = []
    expected = (Right $ Number 3, logs)
  in
    assertEqual "" expected result

test_interpret_expression :: Assertion
test_interpret_expression =
  let
    expr = EVal $ Number 3
    result = evalState (runWriterT $ runExceptT $ interpretExpression expr) emptyState
    logs = []
    expected = (Right $ Number 3, logs)
  in
    assertEqual "" expected result

test_basic_program :: Assertion
test_basic_program =
  let
    program = "a = 2\nb = 3\nfoo = (c, d) => c * d\nbox(b, a, foo(a, b))\n"
    logs = ["Running BuiltIn: box", "Running lambda"]
    maybeGfx = do
      ast <- Language.parse program
      let result = Language.createGfx [] ast
      scene <- fst result
      return $ sceneGfx scene
    result = either (const []) id maybeGfx
    expected = [GA.ShapeCommand (GA.Cube 3 2 6) Nothing]
  in
    assertEqual "" expected result

test_loop_program :: Assertion
test_loop_program =
  let
    program = "rotate(0.1, 0.1, 0.1)\n3 times with i\n\trotate(0.2, 0.2, 0.2)\n\tbox(i)\n\n\n"
    result = do
      ast <- Language.parse program
      scene <- fst $ Language.createGfx [] ast
      return $ sceneGfx scene
    expected = Right [
      GA.MatrixCommand (GA.Rotate 0.1 0.1 0.1) Nothing,
      GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2) Nothing,
      GA.ShapeCommand (GA.Cube 0 1 1) Nothing,
      GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2) Nothing,
      GA.ShapeCommand (GA.Cube 1 1 1) Nothing,
      GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2) Nothing,
      GA.ShapeCommand (GA.Cube 2 1 1) Nothing
      ]
  in
    assertEqual "" expected result

test_create_gfx :: Assertion
test_create_gfx =
  let
    box = EApp $ Application "box" [EVal $ Number 1, EVal $ Number 2, EVal $ Number 1] Nothing
    block = Block [ElExpression box]
    scene = fst $ Language.createGfx [] block :: Either String Scene
    result = either (const []) sceneGfx scene
    expected = [GA.ShapeCommand (GA.Cube 1 2 1) Nothing]
  in
    assertEqual "" expected result
