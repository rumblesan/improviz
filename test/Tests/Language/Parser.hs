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
  testGroup "Parser Tests" [testCase "Parse basic program" test_parse_program]

test_parse_program :: Assertion
test_parse_program =
  let fooDef = "foo = (a, b) =>\n\tc = a + b\n\tbox(c)\n"
      loopNum = "n = (3 * 4) + 1\n"
      loop = "n times\n\trotate(0.5)\n\tfoo(1, 2)\n"
      program = fooDef ++ loopNum ++ loop
      cAss =
        ElAssign $
        Assignment "c" $
        BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")
      fooBox =
        ElExpression $
        EApp $
        Application "box" [ApplicationArg Nothing $ EVar $ Variable "c"] Nothing
      fooLambda =
        Lambda
          [FunctionArg "a" Nothing, FunctionArg "b" Nothing]
          (Block [cAss, fooBox])
      fooLine = ElAssign $ Assignment "foo" $ EVal fooLambda
      nLine =
        ElAssign $
        Assignment "n" $
        BinaryOp
          "+"
          (BinaryOp "*" (EVal $ Number 3) (EVal $ Number 4))
          (EVal $ Number 1)
      loopBlock =
        Block
          [ ElExpression $
            EApp $
            Application
              "rotate"
              [ApplicationArg Nothing $ EVal $ Number 0.5]
              Nothing
          , ElExpression $
            EApp $
            Application
              "foo"
              [ ApplicationArg Nothing $ EVal $ Number 1
              , ApplicationArg Nothing $ EVal $ Number 2
              ]
              Nothing
          ]
      loopLine = ElLoop $ Loop (EVar $ Variable "n") Nothing loopBlock
      expected = Right $ Block [fooLine, nLine, loopLine]
      result = Language.parse program
  in assertEqual "" expected result
