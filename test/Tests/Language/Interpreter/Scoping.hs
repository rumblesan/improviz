module Tests.Language.Interpreter.Scoping (scopingTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Gfx.Ast as GA
import Gfx (Scene(..))
import qualified Language.StdLib as SL
import qualified Language
import Language.LanguageAst
import Language.Interpreter


scopingTests :: Test
scopingTests =
  testGroup "Scoping Tests" [
    testCase "Simple scoping of default" test_basic_default_scoping
  ]

test_basic_default_scoping :: Assertion
test_basic_default_scoping =
  let
    program = "rotate(3, 4, 5)\n\tbox()"
    result = do
      ast <- Language.parse program
      scene <- fst $ Language.createGfx [] ast
      return $ sceneGfx scene
    boxBlock = [GA.ShapeCommand (GA.Cube 1 1 1) Nothing]
    expected = Right [GA.MatrixCommand (GA.Rotate 3 4 5) (Just boxBlock)]
  in
    assertEqual "" expected result

