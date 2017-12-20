module Main where

import           Data.Monoid                            (mempty)

import           Test.Framework

import           Tests.Language                         (languageTests)
import           Tests.Language.Interpreter.Blocks      (blockTests)
import           Tests.Language.Interpreter.Expressions (expressionTests)
import           Tests.Language.Interpreter.If          (interpreterIfTests)
import           Tests.Language.Interpreter.Operators   (operatorTests)
import           Tests.Language.Interpreter.Scoping     (scopingTests)
import           Tests.Language.Parser                  (parserTests)
import           Tests.Language.Parser.Assignment       (parserAssignmentTests)
import           Tests.Language.Parser.Blocks           (parserBlocksTests)
import           Tests.Language.Parser.Collections      (parserCollectionTests)
import           Tests.Language.Parser.ErrorCatching    (parserErrorCatchingTests)
import           Tests.Language.Parser.Functions        (parserFunctionTests)
import           Tests.Language.Parser.If               (parserIfTests)
import           Tests.Language.Parser.Lambda           (parserLambdaTests)
import           Tests.Language.Parser.Loops            (parserLoopTests)
import           Tests.Language.Parser.Symbols          (parserSymbolTests)

main :: IO ()
main =
  defaultMainWithOpts
    [ languageTests
    , operatorTests
    , expressionTests
    , interpreterIfTests
    , blockTests
    , scopingTests
    , parserTests
    , parserAssignmentTests
    , parserBlocksTests
    , parserCollectionTests
    , parserIfTests
    , parserFunctionTests
    , parserLambdaTests
    , parserErrorCatchingTests
    , parserLoopTests
    , parserSymbolTests
    ]
    mempty
