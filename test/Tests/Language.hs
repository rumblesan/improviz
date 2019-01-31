module Tests.Language
  ( languageTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Either
import           Data.Map.Strict
import           Data.Maybe

import           Gfx                            (Scene (..))
import qualified Gfx.Ast                        as GA
import           Gfx.PostProcessing             (AnimationStyle (..))

import qualified Language
import           Language.Ast

languageTests :: Test
languageTests =
  testGroup
    "Language Tests"
    [ testCase "Graphics Creation" test_create_gfx
    , testCase "Animation Style Setting" test_animation_style
    , testCase "Basic program" test_basic_program
    , testCase "Loop program" test_loop_program
    ]

test_basic_program :: Assertion
test_basic_program =
  let program = "a = 2\nb = 3\nfunc foo (c, d) => c * d\nbox(b, a, foo(a, b))\n"
      interpreterState = Language.initialState []
      result = do
        ast <- Language.parse program
        let ((result, _), _) = Language.createGfxScene interpreterState ast
        scene <- result
        return $ sceneGfx scene
      expected = Right [GA.ShapeCommand (GA.Cube 3 2 6)]
   in assertEqual "" expected result

test_animation_style :: Assertion
test_animation_style =
  let program = "motionBlur()"
      interpreterState = Language.initialState []
      result = do
        ast <- Language.parse program
        let ((result, _), _) = Language.createGfxScene interpreterState ast
        scene <- result
        return $ scenePostProcessingFX scene
      expected = Right MotionBlur
   in assertEqual "" expected result

test_loop_program :: Assertion
test_loop_program =
  let program =
        "rotate(0.1, 0.2, 0.3)\n3 times with i\n\trotate(0.2, 0.2, 0.2)\n\tbox(i)\n\n\n"
      interpreterState = Language.initialState []
      result = do
        ast <- Language.parse program
        let ((result, _), _) = Language.createGfxScene interpreterState ast
        scene <- result
        return $ sceneGfx scene
      expected =
        Right
          [ GA.MatrixCommand (GA.Rotate 0.1 0.2 0.3)
          , GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2)
          , GA.ShapeCommand (GA.Cube 0 0 0)
          , GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2)
          , GA.ShapeCommand (GA.Cube 1 1 1)
          , GA.MatrixCommand (GA.Rotate 0.2 0.2 0.2)
          , GA.ShapeCommand (GA.Cube 2 2 2)
          ]
   in assertEqual "" expected result

test_create_gfx :: Assertion
test_create_gfx =
  let box =
        EApp $
        Application
          (LocalVariable "box")
          [EVal $ Number 1, EVal $ Number 2, EVal $ Number 1]
          Nothing
      block = Program [StExpression box]
      interpreterState = Language.initialState []
      ((result, _), _) = Language.createGfxScene interpreterState block
      scene = either (const []) sceneGfx result
      expected = [GA.ShapeCommand (GA.Cube 1 2 1)]
   in assertEqual "" expected scene
