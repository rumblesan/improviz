module Tests.LanguageParser (parserTests) where

import Test.Framework (Test, testGroup)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework.Providers.HUnit (testCase)

import Data.Maybe
import Data.Map.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import qualified Gfx.Ast as GA

import qualified Language
import Language.LanguageAst
import Language.Interpreter


parserTests :: Test
parserTests =
  testGroup "Parser Tests" [
    testCase "Parse basic program" test_parse_program
  ]

test_parse_program :: Assertion
test_parse_program =
  let
    fooDef = "foo = (a b) => {\nc = a + b;\nbox(c)\n};"
    loopNum = "n = (3 * 4) + 1;\n"
    loop = "n times { \nrotate(0.5);\nfoo(1 2);\n}\n"
    program = fooDef ++ loopNum ++ loop

    cAss = ElAssign $ Assignment "c" $ BinaryOp "+" (EVar $ Variable "a") (EVar $ Variable "b")
    fooBox = ElExpression $ EApp $ Application "box" [EVar $ Variable "a"] Nothing
    fooLambda = Lambda ["a", "b"] (Block [cAss, fooBox])
    fooLine = ElAssign $ Assignment "foo" $ EVal fooLambda

    nLine = ElAssign $ Assignment "n" $ BinaryOp "+" (BinaryOp "*" (EVal $ Number 3) (EVal $ Number 4)) (EVal $ Number 4)
    loopBlock = Block [
        ElExpression $ EApp $ Application "rotate" [EVal $ Number 0.5] Nothing,
        ElExpression $ EApp $ Application "foo" [EVal $ Number 1, EVal $ Number 2] Nothing
      ]
    loopLine = ElLoop $ Loop (EVar $ Variable "n") Nothing loopBlock

    expected = Block [fooLine, nLine, loopLine]
    result = either (const $ Block []) id (Language.parse program)
  in
    assertEqual "" expected result

