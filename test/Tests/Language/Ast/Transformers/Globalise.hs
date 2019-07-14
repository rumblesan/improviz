module Tests.Language.Ast.Transformers.Globalise
  ( transformersGlobaliseTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion
                                                , assertEqual
                                                )

import qualified Data.Set                      as S

import           Language.Ast
import           Language.Ast.Transformers.Globalise

transformersGlobaliseTests :: Test
transformersGlobaliseTests = testGroup
  "AST Transformer Variable Globalisation Tests"
  [ testCase "simple variable to global variable" test_var_to_global
  , testCase "func body globalised"               test_func_var_to_global
  , testCase "overwriting globals ok"             test_overwrite_global
  , testCase "overwriting with loop var"          test_overwrite_with_loop
  ]

test_var_to_global :: Assertion
test_var_to_global =
  let program = Program
        [ StExpression $ BinaryOp "+"
                                  (EVar $ LocalVariable "foo")
                                  (EVar $ LocalVariable "bar")
        ]
      expected = Program
        [ StExpression $ BinaryOp "+"
                                  (EVar $ GlobalVariable "foo")
                                  (EVar $ LocalVariable "bar")
        ]
      globals = S.singleton "foo"
      result  = globalise globals program
  in  assertEqual "" expected result

test_func_var_to_global :: Assertion
test_func_var_to_global =
  let beforeBlock = Block
        [ ElAssign $ AbsoluteAssignment
          "b"
          (BinaryOp "*" (EVar $ LocalVariable "time") (EVal $ Number 3))
        , ElAssign $ AbsoluteAssignment
          "c"
          (BinaryOp "*"
                    (EVar $ LocalVariable "arg1")
                    (EVar $ LocalVariable "time")
          )
        , ElExpression
          $ BinaryOp "+" (EVar $ LocalVariable "b") (EVar $ LocalVariable "c")
        ]
      program = Program
        [ StFunc $ Func "myfunc" [VarArg "arg1"] beforeBlock
        , StExpression $ EApp $ Application (LocalVariable "myfunc")
                                            [EVal $ Number 4]
                                            Nothing
        ]
      afterBlock = Block
        [ ElAssign $ AbsoluteAssignment
          "b"
          (BinaryOp "*" (EVar $ GlobalVariable "time") (EVal $ Number 3))
        , ElAssign $ AbsoluteAssignment
          "c"
          (BinaryOp "*"
                    (EVar $ LocalVariable "arg1")
                    (EVar $ GlobalVariable "time")
          )
        , ElExpression
          $ BinaryOp "+" (EVar $ LocalVariable "b") (EVar $ LocalVariable "c")
        ]
      expected = Program
        [ StFunc $ Func "myfunc" [VarArg "arg1"] afterBlock
        , StExpression $ EApp $ Application (GlobalVariable "myfunc")
                                            [EVal $ Number 4]
                                            Nothing
        ]
      globals = S.singleton "time"
      result  = globalise globals program
  in  assertEqual "" expected result

test_overwrite_global :: Assertion
test_overwrite_global =
  let
    program = Program
      [ StExpression
        $ BinaryOp "+" (EVar $ LocalVariable "foo") (EVar $ LocalVariable "bar")
      , StAssign $ AbsoluteAssignment "foo" (EVal $ Number 3)
      , StExpression
        $ BinaryOp "+" (EVar $ LocalVariable "foo") (EVar $ LocalVariable "bar")
      ]
    expected = Program
      [ StExpression $ BinaryOp "+"
                                (EVar $ GlobalVariable "foo")
                                (EVar $ LocalVariable "bar")
      , StAssign $ AbsoluteAssignment "foo" (EVal $ Number 3)
      , StExpression $ BinaryOp "+"
                                (EVar $ LocalVariable "foo")
                                (EVar $ LocalVariable "bar")
      ]
    globals = S.singleton "foo"
    result  = globalise globals program
  in
    assertEqual "" expected result

test_overwrite_with_loop :: Assertion
test_overwrite_with_loop =
  let program = Program
        [ StExpression $ BinaryOp "+"
                                  (EVar $ LocalVariable "foo")
                                  (EVar $ LocalVariable "bar")
        , StLoop $ Loop
          (EVal $ Number 3)
          (Just "foo")
          (Block
            [ ElExpression $ BinaryOp "+"
                                      (EVar $ LocalVariable "foo")
                                      (EVar $ LocalVariable "bar")
            ]
          )
        , StExpression $ BinaryOp "+"
                                  (EVar $ LocalVariable "foo")
                                  (EVar $ LocalVariable "bar")
        ]
      expected = Program
        [ StExpression $ BinaryOp "+"
                                  (EVar $ GlobalVariable "foo")
                                  (EVar $ LocalVariable "bar")
        , StLoop $ Loop
          (EVal $ Number 3)
          (Just "foo")
          (Block
            [ ElExpression $ BinaryOp "+"
                                      (EVar $ LocalVariable "foo")
                                      (EVar $ LocalVariable "bar")
            ]
          )
        , StExpression $ BinaryOp "+"
                                  (EVar $ GlobalVariable "foo")
                                  (EVar $ LocalVariable "bar")
        ]
      globals = S.singleton "foo"
      result  = globalise globals program
  in  assertEqual "" expected result
