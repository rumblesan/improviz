{-# LANGUAGE OverloadedStrings #-}

module Configuration.Ast where

import Language.Ast (Value(..))
import           Data.Yaml                      ( FromJSON(..))

import qualified Data.Vector                   as V
import Data.Scientific (toRealFloat)
import qualified Data.Yaml                     as Y

instance FromJSON Value where
  parseJSON (Y.Number v) = return (Number $ toRealFloat v)
  parseJSON (Y.Array v)  = VList <$> mapM parseJSON (V.toList v)
  parseJSON _            = fail "Expected Number or Array for Config value"
