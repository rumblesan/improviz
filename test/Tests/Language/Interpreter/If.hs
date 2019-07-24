module Tests.Language.Interpreter.If
  ( interpreterIfTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( gfxTest )
import qualified TestHelpers.GfxAst            as GA

interpreterIfTests :: Test
interpreterIfTests = testGroup
  "If Tests"
  [ testCase "interprets true single if statement"  test_true_if_statement
  , testCase "interprets false single if statement" test_false_if_statement
  , testCase "interprets true if else statement"    test_true_if_else_statement
  , testCase "interprets false if else statement"   test_false_if_else_statement
  ]

test_true_if_statement :: Assertion
test_true_if_statement =
  let program     = "if (1)\n\tshape(:cube, 1, 1, 1)"
      expectedGfx = [GA.ShapeCommand (GA.ShapeGfx "cube" 1 1 1)]
  in  gfxTest program expectedGfx

test_false_if_statement :: Assertion
test_false_if_statement =
  let program     = "if (0)\n\tshape(:cube, 1, 1, 1)"
      expectedGfx = []
  in  gfxTest program expectedGfx

test_true_if_else_statement :: Assertion
test_true_if_else_statement =
  let program =
          "if (1)\n\tshape(:cube, 1, 1, 1)\nelse\n\tshape(:line, 1, 1, 1)"
      expectedGfx = [GA.ShapeCommand (GA.ShapeGfx "cube" 1 1 1)]
  in  gfxTest program expectedGfx

test_false_if_else_statement :: Assertion
test_false_if_else_statement =
  let program =
          "if (0)\n\tshape(:cube, 1, 1, 1)\nelse\n\tshape(:line, 1, 1, 1)"
      expectedGfx = [GA.ShapeCommand (GA.ShapeGfx "line" 1 1 1)]
  in  gfxTest program expectedGfx
