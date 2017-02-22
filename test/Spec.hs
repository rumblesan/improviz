module Main where

import Data.Monoid (mempty)

import Test.Framework

import Tests.LCLangLite (lclangLiteTests)
import Tests.LanguageParser (lclangParserTests)

main :: IO ()
main = defaultMainWithOpts
  [
    lclangLiteTests,
    lclangParserTests
  ]
  mempty
