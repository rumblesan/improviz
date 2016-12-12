module Main where

import Data.Monoid (mempty)

import Test.Framework

main :: IO ()
main = defaultMainWithOpts [] mempty
