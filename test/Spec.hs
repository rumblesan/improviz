module Main where

import           Data.Monoid                    ( mempty )

import           Test.Framework

import           Tests.Language.Ast.Transformers.Globalise
                                                ( globaliseTransformerTests )
import           Tests.Language.Interpreter.Blocks
                                                ( blockTests )
import           Tests.Language.Interpreter.Expressions
                                                ( expressionTests )
import           Tests.Language.Interpreter.Functions
                                                ( interpreterFunctionTests )
import           Tests.Language.Interpreter.If  ( interpreterIfTests )
import           Tests.Language.Interpreter.Loops
                                                ( loopTests )
import           Tests.Language.Interpreter.Operators
                                                ( operatorTests )
import           Tests.Language.Interpreter.Scoping
                                                ( scopingTests )
import           Tests.Language.Parser          ( parserTests )
import           Tests.Language.Parser.Assignment
                                                ( parserAssignmentTests )
import           Tests.Language.Parser.Blocks   ( parserBlocksTests )
import           Tests.Language.Parser.ErrorCatching
                                                ( parserErrorCatchingTests )
import           Tests.Language.Parser.Functions
                                                ( parserFunctionTests )
import           Tests.Language.Parser.If       ( parserIfTests )
import           Tests.Language.Parser.Loops    ( parserLoopTests )
import           Tests.Language.Parser.Operators
                                                ( parserOperatorTests )
import           Tests.Language.Parser.Symbols  ( parserSymbolTests )

import           Tests.Language.VM              ( vmTests )

main :: IO ()
main = defaultMainWithOpts
  [ globaliseTransformerTests
  , operatorTests
  , expressionTests
  , interpreterIfTests
  , interpreterFunctionTests
  , loopTests
  , blockTests
  , scopingTests
  , parserTests
  , parserAssignmentTests
  , parserBlocksTests
  , parserIfTests
  , parserFunctionTests
  , parserErrorCatchingTests
  , parserLoopTests
  , parserOperatorTests
  , parserSymbolTests
  , vmTests
  ]
  mempty
