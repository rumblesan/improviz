module Tests.Language.Interpreter.Scoping
  ( scopingTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( gfxTest )

import qualified Gfx.Ast                       as GA

scopingTests :: Test
scopingTests = testGroup
  "Scoping Tests"
  [testCase "Simple scoping of default" test_basic_default_scoping]

test_basic_default_scoping :: Assertion
test_basic_default_scoping =
  let
    program
      = "pushScope()\n\
         \matrix(:rotate, 3, 4, 5)\n\
         \shape(:cube, 1, 1, 1)\n\
         \popScope()"
    expectedGfx =
      [ GA.ScopeCommand GA.PushScope
      , GA.MatrixCommand (GA.Rotate 3 4 5)
      , GA.ShapeCommand (GA.Cube 1 1 1)
      , GA.ScopeCommand GA.PopScope
      ]
  in
    gfxTest program expectedGfx
