module Tests.Language.Interpreter.Blocks (blockTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import qualified Gfx.Ast as GA
import Gfx (Scene(..))
import qualified Language.StdLib as SL
import qualified Language
import Language.LanguageAst
import Language.Interpreter


blockTests :: Test
blockTests =
  testGroup "Block Tests" [
    testCase "Simple block tests" test_basic_block_calling
  ]

test_basic_block_calling :: Assertion
test_basic_block_calling =
  let
    program = "a = (sizeX, sizeY) =>\n\tscale(sizeX, sizeY)\n\trunBlock()\n\na(2, 1)\n\tbox(1)"
    result = do
      ast <- Language.parse program
      scene <- fst $ Language.createGfx [] ast
      return $ sceneGfx scene
    expected = Right [
        GA.MatrixCommand (GA.Scale 2 1 1) Nothing,
        GA.ShapeCommand (GA.Cube 1 1 1) Nothing
      ]
  in
    assertEqual "" expected result

