module Tests.GfxInterpreter where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import GfxAst
import GfxInterpreter

gfxInterpreterTests :: Test
gfxInterpreterTests =
  testGroup "GFX Interpreter Tests" [
    testCase "Simple interpretation works as expected" test_simple
  ]

test_simple :: Assertion
test_simple =
  let
    shape = Cube 1 1 1
    shapeblock = Just $ [ShapeCommand shape Nothing]
    matrix = Rotate 1 1 1
    ast = [MatrixCommand matrix shapeblock]
    expected = [(show matrix) ++ " scope enter", (show shape), (show matrix) ++ " scope leave"]
  in
    assertEqual "" expected $ interpretGfx ast
