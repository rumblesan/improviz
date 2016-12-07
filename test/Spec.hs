module Main where

import Data.Monoid

import Test.Framework

import Tests.GfxInterpreter (gfxInterpreterTests)

main :: IO ()
main =
  defaultMainWithOpts
    [
      gfxInterpreterTests
    ]
    mempty
