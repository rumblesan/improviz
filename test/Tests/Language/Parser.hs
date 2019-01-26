module Tests.Language.Parser
  ( parserTests
  ) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertEqual)

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Map.Strict
import           Data.Maybe

import qualified Gfx.Ast                        as GA

import qualified Language
import           Language.Ast
import           Language.Interpreter

parserTests :: Test
parserTests =
  testGroup
    "Parser Tests"
    [ testCase "Parse basic program" test_parse_program
    , testCase "Parse blank program" test_parse_blank_program
    , testCase "Parse empty program" test_parse_empty_program
    ]

test_parse_program :: Assertion
test_parse_program =
  let fooDef = "func foo (a, b) =>\n\tvar c = a + b\n\tbox(c)\n"
      loopNum = "var n := (3 * 4) + 1\n"
      loop = "n times\n\trotate(0.5)\n\tfoo(1, 2)\n"
      program = fooDef ++ loopNum ++ loop
      cAss =
        ElAssign $
        AbsoluteAssignment "c" $
        BinaryOp "+" (EVar $ LocalVariable "a") (EVar $ LocalVariable "b")
      fooBox =
        ElExpression $
        EApp $
        Application (LocalVariable "box") [EVar $ LocalVariable "c"] Nothing
      fooLine = StFunc $ Func "foo" ["a", "b"] (Block [cAss, fooBox])
      nLine =
        StAssign $
        ConditionalAssignment "n" $
        BinaryOp
          "+"
          (BinaryOp "*" (EVal $ Number 3) (EVal $ Number 4))
          (EVal $ Number 1)
      loopBlock =
        Block
          [ ElExpression $
            EApp $
            Application (LocalVariable "rotate") [EVal $ Number 0.5] Nothing
          , ElExpression $
            EApp $
            Application
              (LocalVariable "foo")
              [EVal $ Number 1, EVal $ Number 2]
              Nothing
          ]
      loopLine = StLoop $ Loop (EVar $ LocalVariable "n") Nothing loopBlock
      expected = Right $ Program [fooLine, nLine, loopLine]
      result = Language.parse program
   in assertEqual "" expected result

test_parse_blank_program :: Assertion
test_parse_blank_program =
  let program = ""
      expected = Right $ Program []
      result = Language.parse program
   in assertEqual "" expected result

test_parse_empty_program :: Assertion
test_parse_empty_program =
  let program = "   \n    \n    "
      expected = Right $ Program []
      result = Language.parse program
   in assertEqual "" expected result
