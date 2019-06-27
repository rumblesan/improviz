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

import           Gfx                            ( Scene(..) )
import qualified Gfx.Ast                       as GA

import qualified Language
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
    interpreterState = Language.initialState []
    result           = do
      ast <- Language.simpleParse program
      let result = fst $ Language.createGfxScene interpreterState ast
      sceneGfx <$> result
    expected = Right [GA.ShapeCommand (GA.Cube 3 2 6)]
  in
    assertEqual "" expected result

test_loop_program :: Assertion
test_loop_program =
  let
    program
      = "matrix(:rotate, 0.1, 0.2, 0.3)\n3 times with i\n\tmatrix(:rotate, 0.2, 0.2, 0.2)\n\tshape(:cube, i, i, i)\n\n\n"
    interpreterState = Language.initialState []
    result           = do
      ast <- Language.simpleParse program
      let result = fst $ Language.createGfxScene interpreterState ast
      sceneGfx <$> result
    expected = Right
      [ GA.MatrixCommand (GA.Rotate 0.1 0.2 0.3)
      , GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2)
      , GA.ShapeCommand (GA.Cube 0 0 0)
      , GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2)
      , GA.ShapeCommand (GA.Cube 1 1 1)
      , GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2)
      , GA.ShapeCommand (GA.Cube 2 2 2)
      ]
  in
    assertEqual "" expected result

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
      block            = Program [StExpression cube]
      interpreterState = Language.initialState []
      result           = fst $ Language.createGfxScene interpreterState block
      scene            = either (const []) sceneGfx result
      expected         = [GA.ShapeCommand (GA.Cube 1 2 1)]
  in  assertEqual "" expected scene
