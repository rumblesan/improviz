module Tests.Language.Parser
  ( parserTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion
                                                , assertEqual
                                                )

import qualified Language
import           Language.Ast

parserTests :: Test
parserTests = testGroup
  "Parser Tests"
  [ testCase "Parse basic program"        test_parse_program
  , testCase "Parse blank program"        test_parse_blank_program
  , testCase "Parse empty program"        test_parse_empty_program
  , testCase "Parse nested indent blocks" test_nested_indent_blocks
  ]

test_parse_program :: Assertion
test_parse_program =
  let
    fooDef  = "func foo (a, b)\n\tc = a + b\n\tbox(c)\n"
    loopNum = "n := (3 * 4) + 1\n"
    loop    = "n times\n\trotate(0.5)\n\tfoo(1, 2)\n"
    program = fooDef ++ loopNum ++ loop
    cAss    = ElAssign $ AbsoluteAssignment "c" $ BinaryOp
      "+"
      (EVar $ LocalVariable "a")
      (EVar $ LocalVariable "b")
    fooBox = ElExpression $ EApp $ Application (LocalVariable "box")
                                               [EVar $ LocalVariable "c"]
                                               Nothing
    fooLine =
      StFunc $ Func "foo" [VarArg "a", VarArg "b"] (Block [cAss, fooBox])
    nLine = StAssign $ ConditionalAssignment "n" $ BinaryOp
      "+"
      (BinaryOp "*" (EVal $ Number 3) (EVal $ Number 4))
      (EVal $ Number 1)
    loopBlock = Block
      [ ElExpression $ EApp $ Application (LocalVariable "rotate")
                                          [EVal $ Number 0.5]
                                          Nothing
      , ElExpression $ EApp $ Application (LocalVariable "foo")
                                          [EVal $ Number 1, EVal $ Number 2]
                                          Nothing
      ]
    loopLine = StLoop $ Loop (EVar $ LocalVariable "n") Nothing loopBlock
    expected = Right $ Program [fooLine, nLine, loopLine]
    result   = Language.parse program
  in
    assertEqual "" expected result

test_parse_blank_program :: Assertion
test_parse_blank_program =
  let program  = ""
      expected = Right $ Program []
      result   = Language.parse program
  in  assertEqual "" expected result

test_parse_empty_program :: Assertion
test_parse_empty_program =
  let program  = "   \n    \n    "
      expected = Right $ Program []
      result   = Language.parse program
  in  assertEqual "" expected result

test_nested_indent_blocks :: Assertion
test_nested_indent_blocks =
  let
    code
      = "10 times with x\n\
           \\trotate()\n\
           \\t10 times with y\n\
           \\t\trotate()\n\
           \\t\tbox()"
    ast = Program
      [ StLoop $ Loop (EVal $ Number 10) (Just "x") $ Block
          [ ElExpression $ EApp $ Application (LocalVariable "rotate")
                                              []
                                              Nothing
          , ElLoop $ Loop (EVal $ Number 10) (Just "y") $ Block
            [ ElExpression $ EApp $ Application (LocalVariable "rotate")
                                                []
                                                Nothing
            , ElExpression $ EApp $ Application (LocalVariable "box") [] Nothing
            ]
          ]
      ]
    expected = Right ast
    result   = Language.parse code
  in
    assertEqual "" expected result
