module Main where

import           Data.Monoid                    ( mempty )

import           Test.Framework

import           Tests.Language.Ast.Transformers.Globalise
                                                ( transformersGlobaliseTests )
import           Tests.Language.Interpreter.Blocks
                                                ( interpreterBlockTests )
import           Tests.Language.Interpreter.Expressions
                                                ( interpreterExpressionTests )
import           Tests.Language.Interpreter.Functions
                                                ( interpreterFunctionTests )
import           Tests.Language.Interpreter.If  ( interpreterIfTests )
import           Tests.Language.Interpreter.Lists
                                                ( interpreterListTests )
import           Tests.Language.Interpreter.Loops
                                                ( loopTests )
import           Tests.Language.Interpreter.Operators
                                                ( operatorTests )
import           Tests.Language.Interpreter.Scoping
                                                ( scopingTests )
import           Tests.Language.Parser          ( parserTests )

import           Tests.Language.VM              ( vmTests )

main :: IO ()
main = defaultMainWithOpts
  [ transformersGlobaliseTests
  , operatorTests
  , interpreterExpressionTests
  , interpreterIfTests
  , interpreterFunctionTests
  , interpreterListTests
  , loopTests
  , interpreterBlockTests
  , scopingTests
  , parserTests
  , vmTests
  ]
  mempty
