module Tests.Language.Interpreter
  ( interpreterTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )

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
                                                ( interpreterLoopTests )
import           Tests.Language.Interpreter.Operators
                                                ( interpreterOperatorTests )
import           Tests.Language.Interpreter.Scoping
                                                ( interpreterScopingTests )

interpreterTests :: Test
interpreterTests = testGroup
  "Interpreter Tests"
  [ interpreterBlockTests
  , interpreterExpressionTests
  , interpreterFunctionTests
  , interpreterIfTests
  , interpreterListTests
  , interpreterLoopTests
  , interpreterOperatorTests
  , interpreterScopingTests
  ]
