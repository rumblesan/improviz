module Tests.Language.Parser.Blocks
  ( parserBlocksTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.HUnit                     ( Assertion )
import           TestHelpers.Util               ( parserTest )

import           Language.Ast

parserBlocksTests :: Test
parserBlocksTests = testGroup
  "Blocks Tests"
  [ testCase "parses block with blank lines" test_parses_block_with_blank_lines
  , testCase "parses nested indent blocks"   test_parses_nested_indent_blocks
  ]

test_parses_block_with_blank_lines :: Assertion
test_parses_block_with_blank_lines =
  let program = "\n\nfill(255, 0, 0)\n\n\trotate()\n\t\n\tbox()\n\n"
      rotate =
          ElExpression $ EApp $ Application (LocalVariable "rotate") [] Nothing
      box  = ElExpression $ EApp $ Application (LocalVariable "box") [] Nothing
      fill = StExpression $ EApp $ Application
        (LocalVariable "fill")
        [ ApplicationSingleArg $ EVal (Number 255)
        , ApplicationSingleArg $ EVal (Number 0)
        , ApplicationSingleArg $ EVal (Number 0)
        ]
        (Just $ Lambda [] Nothing $ Block [rotate, box])
      expected = Program [fill]
  in  parserTest program expected

test_parses_nested_indent_blocks :: Assertion
test_parses_nested_indent_blocks =
  let
    program
      = "10 times with x\n\
           \\trotate()\n\
           \\t10 times with y\n\
           \\t\trotate()\n\
           \\t\tbox()"
    expected = Program
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
  in
    parserTest program expected
