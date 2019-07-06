module Tests.Language.Interpreter.Blocks
  ( blockTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )

import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( gfxTest )

import qualified Gfx.Ast                       as GA

blockTests :: Test
blockTests = testGroup
  "Block Tests"
  [testCase "Simple block tests" test_basic_block_calling]

test_basic_block_calling :: Assertion
test_basic_block_calling =
  let
    program
      = "func a (sizeX, sizeY, &blk)\n\
        \\tmatrix(:scale, sizeX, sizeY, sizeY)\n\
        \\tblk()\n\
        \a(2, 1)\n\
        \\tshape(:cube, 1, 1, 1)"
    expectedGfx =
      [GA.MatrixCommand (GA.Scale 2 1 1), GA.ShapeCommand (GA.Cube 1 1 1)]
  in
    gfxTest program expectedGfx
