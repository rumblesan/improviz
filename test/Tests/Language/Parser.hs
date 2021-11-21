module Tests.Language.Parser
  ( parserTests
  )
where

import           Test.Framework                 ( Test
                                                , testGroup
                                                )

import           Tests.Language.Parser.Assignment
                                                ( parserAssignmentTests )
import           Tests.Language.Parser.Blocks   ( parserBlocksTests )
import           Tests.Language.Parser.Comments   ( parserCommentTests )
import           Tests.Language.Parser.ErrorCatching
                                                ( parserErrorCatchingTests )
import           Tests.Language.Parser.Functions
                                                ( parserFunctionTests )
import           Tests.Language.Parser.General  ( parserGeneralTests )
import           Tests.Language.Parser.If       ( parserIfTests )
import           Tests.Language.Parser.Lists    ( parserListTests )
import           Tests.Language.Parser.Loops    ( parserLoopTests )
import           Tests.Language.Parser.Operators
                                                ( parserOperatorTests )
import           Tests.Language.Parser.Symbols  ( parserSymbolTests )

parserTests :: Test
parserTests = testGroup
  "Parser Tests"
  [ parserAssignmentTests
  , parserBlocksTests
  , parserCommentTests
  , parserErrorCatchingTests
  , parserFunctionTests
  , parserGeneralTests
  , parserIfTests
  , parserListTests
  , parserLoopTests
  , parserOperatorTests
  , parserSymbolTests
  ]
