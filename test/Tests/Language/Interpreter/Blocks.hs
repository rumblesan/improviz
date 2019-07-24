module Tests.Language.Interpreter.Blocks
  ( interpreterBlockTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )

import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( gfxTest )
import qualified TestHelpers.GfxAst            as GA


interpreterBlockTests :: Test
interpreterBlockTests = testGroup
  "Block Tests"
  [testCase "interprets application block" test_application_block]

test_application_block :: Assertion
test_application_block =
  let
    program
      = "func a (sizeX, sizeY, &blk)\n\
        \\tmatrix(:scale, sizeX, sizeY, sizeY)\n\
        \\tblk()\n\
        \a(2, 1)\n\
        \\tshape(:cube, 1, 1, 1)"
    expectedGfx =
      [ GA.MatrixCommand (GA.Scale 2 1 1)
      , GA.ShapeCommand (GA.ShapeGfx "cube" 1 1 1)
      ]
  in
    gfxTest program expectedGfx
