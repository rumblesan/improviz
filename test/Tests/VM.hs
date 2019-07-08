module Tests.VM
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

import           Language.ImpVM.Types           ( VMState(..)
                                                , StackItem(..)
                                                )
import qualified Gfx.Ast                       as GA

vmTests :: Test
vmTests = testGroup
  "VM Tests"
  [ testCase "Simple addition" test_basic_addition
  , testCase "Simple gfx"      test_vm_gfx
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
      expectedGfx = [GA.ShapeCommand (GA.Cube 1 1 1)]
  in  vmGfxTest program M.empty expectedGfx
