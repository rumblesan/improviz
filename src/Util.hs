module Util where

import           Control.Monad                  ( unless )

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
  b <- action
  unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
  Nothing -> nothingRes
  Just x  -> f x

maybeElem :: Int -> [a] -> Maybe a
maybeElem _ []       = Nothing
maybeElem 0 (x : xs) = Just x
maybeElem i (_ : xs) = maybeElem (i - 1) xs

(/.) :: Int -> Int -> Float
(/.) a b = (fromIntegral a) / (fromIntegral b)
