module Tests.Language.VM
  ( vmTests
  )
where

import qualified Data.Map.Strict               as M
import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion
                                                , assertEqual
                                                )
import           TestHelpers.Util               ( vmResultTest
                                                , vmGfxTest
                                                )
import qualified TestHelpers.GfxAst            as GA

import           Language.ImpVM.Types           ( VMState(..)
                                                , StackItem(..)
                                                )

vmTests :: Test
vmTests = testGroup
  "VM Tests"
  [ testCase "Simple addition" test_basic_addition
  , testCase "Simple gfx"      test_vm_gfx
  , testCase "Complex gfx"     test_complex_gfx
  ]

test_basic_addition :: Assertion
test_basic_addition =
  let program  = "a = 3\nb = 4\na + 1 + b"
      expected = SFloat 8
      check vm = assertEqual "" expected (head $ _opstack vm)
  in  vmResultTest program M.empty check

test_vm_gfx :: Assertion
test_vm_gfx =
  let program     = "shape(:cube, 1, 1, 1)"
      expectedGfx = [GA.ShapeCommand (GA.ShapeGfx "cube" 1 1 1)]
  in  vmGfxTest program M.empty expectedGfx

test_complex_gfx :: Assertion
test_complex_gfx =
  let
    program
      = "\
    \pushScope()\n\
    \style(:fill, 255, 0, 0, 0)\n\
    \matrix(:rotate, time, time, 1)\n\
    \popScope()"
    extVars = M.fromList [("time", SFloat 7)]
    expectedGfx =
      [ GA.ScopeCommand GA.PushScope
      , GA.ColourCommand $ GA.Fill $ GA.ColourStyle 1 0 0 0
      , GA.MatrixCommand $ GA.Rotate 7 7 1
      , GA.ScopeCommand GA.PopScope
      ]
  in
    vmGfxTest program extVars expectedGfx
