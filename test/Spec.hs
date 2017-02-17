module Main where

import Data.Monoid (mempty)

import Test.Framework

import Tests.LCLangLite (lclangLiteTests)

main :: IO ()
main = defaultMainWithOpts
  [
    lclangLiteTests
  ]
  mempty
