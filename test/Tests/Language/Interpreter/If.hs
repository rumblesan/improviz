module Tests.Language.Interpreter.If
  ( interpreterIfTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion
                                                , assertEqual
                                                )

import           Gfx                            ( Scene(..) )
import qualified Gfx.Ast                       as GA
import qualified Language

interpreterIfTests :: Test
interpreterIfTests = testGroup
  "If Tests"
  [ testCase "True single if statement"  test_true_if_statement
  , testCase "False single if statement" test_false_if_statement
  , testCase "True if else statement"    test_true_if_else_statement
  , testCase "False if else statement"   test_false_if_else_statement
  ]

test_true_if_statement :: Assertion
test_true_if_statement =
  let program = "if (1)\n\tshape(:cube, 1, 1, 1)"
      result  = do
        ast <- Language.parse program
        let result =
              fst $ Language.createGfxScene (Language.initialState []) ast
        scene <- result
        return $ sceneGfx scene
      expected = Right [GA.ShapeCommand (GA.Cube 1 1 1)]
  in  assertEqual "" expected result

test_false_if_statement :: Assertion
test_false_if_statement =
  let program = "if (0)\n\tshape(:cube, 1, 1, 1)"
      result  = do
        ast <- Language.parse program
        let result =
              fst $ Language.createGfxScene (Language.initialState []) ast
        scene <- result
        return $ sceneGfx scene
      expected = Right []
  in  assertEqual "" expected result

test_true_if_else_statement :: Assertion
test_true_if_else_statement =
  let program = "if (1)\n\tshape(:cube, 1, 1, 1)\nelse\n\tshape(:line, 1)"
      result  = do
        ast <- Language.parse program
        let result =
              fst $ Language.createGfxScene (Language.initialState []) ast
        scene <- result
        return $ sceneGfx scene
      expected = Right [GA.ShapeCommand (GA.Cube 1 1 1)]
  in  assertEqual "" expected result

test_false_if_else_statement :: Assertion
test_false_if_else_statement =
  let program = "if (0)\n\tshape(:cube, 1, 1, 1)\nelse\n\tshape(:line, 1)"
      result  = do
        ast <- Language.parse program
        let result =
              fst $ Language.createGfxScene (Language.initialState []) ast
        scene <- result
        return $ sceneGfx scene
      expected = Right [GA.ShapeCommand (GA.Line 1)]
  in  assertEqual "" expected result
