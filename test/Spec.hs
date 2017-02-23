module Main where

import Data.Monoid (mempty)

import Test.Framework

import Tests.Language (languageTests)
import Tests.LanguageParser (parserTests)

main :: IO ()
main = defaultMainWithOpts
  [
    languageTests,
    parserTests
  ]
  mempty
