{-# LANGUAGE OverloadedStrings #-}

module Configuration.Ast where

import           Data.Yaml                      ( FromJSON(..) )
import           Language.Ast                   ( Value(..) )

import           Data.Scientific                ( toRealFloat )
import qualified Data.Vector                   as V
import qualified Data.Yaml                     as Y

instance FromJSON Value where
  parseJSON (Y.Number v) = return (Number $ toRealFloat v)
  parseJSON (Y.Array  v) = VList <$> mapM parseJSON (V.toList v)
  parseJSON _            = fail "Expected Number or Array for Config value"
