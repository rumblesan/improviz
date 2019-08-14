module Tests.Language.Parser.General
  ( parserGeneralTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( parserTest )

import           Language.Ast

parserGeneralTests :: Test
parserGeneralTests = testGroup
  "General Tests"
  [ testCase "parses basic program" test_parses_program
  , testCase "parses blank program" test_parses_blank_program
  , testCase "parses empty program" test_parses_empty_program
  ]

test_parses_program :: Assertion
test_parses_program =
  let
    fooDef  = "func foo (a, b)\n\tc = a + b\n\tbox(c)\n"
    loopNum = "n := (3 * 4) + 1\n"
    loop    = "n times\n\trotate(0.5)\n\tfoo(1, 2)\n"
    program = fooDef ++ loopNum ++ loop
    cAss    = ElAssign $ AbsoluteAssignment "c" $ BinaryOp
      "+"
      (EVar $ LocalVariable "a")
      (EVar $ LocalVariable "b")
    fooBox = ElExpression $ EApp $ Application
      (LocalVariable "box")
      [ApplicationSingleArg $ EVar $ LocalVariable "c"]
      Nothing
    fooLine =
      StFunc $ Func "foo" [VarArg "a", VarArg "b"] (Block [cAss, fooBox])
    nLine = StAssign $ ConditionalAssignment "n" $ BinaryOp
      "+"
      (BinaryOp "*" (EVal $ Number 3) (EVal $ Number 4))
      (EVal $ Number 1)
    loopBlock = Block
      [ ElExpression $ EApp $ Application
        (LocalVariable "rotate")
        [ApplicationSingleArg $ EVal $ Number 0.5]
        Nothing
      , ElExpression $ EApp $ Application
        (LocalVariable "foo")
        [ ApplicationSingleArg $ EVal $ Number 1
        , ApplicationSingleArg $ EVal $ Number 2
        ]
        Nothing
      ]
    loopLine = StLoop $ Loop (EVar $ LocalVariable "n") Nothing loopBlock
    expected = Program [fooLine, nLine, loopLine]
  in
    parserTest program expected

test_parses_blank_program :: Assertion
test_parses_blank_program =
  let program  = ""
      expected = Program []
  in  parserTest program expected

test_parses_empty_program :: Assertion
test_parses_empty_program =
  let program  = "   \n    \n    "
      expected = Program []
  in  parserTest program expected
