module Tests.Language
  ( languageTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion
                                                , assertEqual
                                                )
import           TestHelpers.GfxContext         ( createGfxContextHelpers
                                                , getOutputGfx
                                                )

import qualified Gfx.Ast                       as GA

import qualified Language                      as L
import           Language.Ast

languageTests :: Test
languageTests = testGroup
  "Language Tests"
  [ testCase "Basic program"     test_basic_program
  , testCase "Loop program"      test_loop_program
  , testCase "Graphics Creation" test_create_gfx
  ]

test_basic_program :: Assertion
test_basic_program =
  let
    program =
      "a = 2\nb = 3\nfunc foo (c, d) => c * d\nshape(:cube, b, a, foo(a, b))\n"
    (Right ast) = L.simpleParse program
    expectedGfx = [GA.ShapeCommand (GA.Cube 3 2 6)]
  in
    do
      (out, ctx)       <- createGfxContextHelpers
      interpreterState <- L.initialState [] ctx
      result           <- fst <$> L.interpret interpreterState ast
      assertEqual "interpreter runs" (Right Null) result
      gfx <- getOutputGfx out
      assertEqual "correct GFX" gfx expectedGfx

test_loop_program :: Assertion
test_loop_program =
  let
    program
      = "matrix(:rotate, 0.1, 0.2, 0.3)\n3 times with i\n\tmatrix(:rotate, 0.2, 0.2, 0.2)\n\tshape(:cube, i, i, i)\n\n\n"
    (Right ast) = L.simpleParse program
    expectedGfx =
      [ GA.MatrixCommand (GA.Rotate 0.1 0.2 0.3)
      , GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2)
      , GA.ShapeCommand (GA.Cube 0 0 0)
      , GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2)
      , GA.ShapeCommand (GA.Cube 1 1 1)
      , GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2)
      , GA.ShapeCommand (GA.Cube 2 2 2)
      ]
  in
    do
      (out, ctx)       <- createGfxContextHelpers
      interpreterState <- L.initialState [] ctx
      result           <- fst <$> L.interpret interpreterState ast
      assertEqual "interpreter runs" (Right Null) result
      gfx <- getOutputGfx out
      assertEqual "correct GFX" gfx expectedGfx

test_create_gfx :: Assertion
test_create_gfx =
  let cube = EApp $ Application
        (LocalVariable "shape")
        [ EVal $ Symbol "cube"
        , EVal $ Number 1
        , EVal $ Number 2
        , EVal $ Number 1
        ]
        Nothing
      block       = Program [StExpression cube]
      expectedGfx = [GA.ShapeCommand (GA.Cube 1 2 1)]
  in  do
        (out, ctx)       <- createGfxContextHelpers
        interpreterState <- L.initialState [] ctx
        result           <- fst <$> L.interpret interpreterState block
        assertEqual "interpreter runs" (Right Null) result
        gfx <- getOutputGfx out
        assertEqual "correct GFX" gfx expectedGfx
