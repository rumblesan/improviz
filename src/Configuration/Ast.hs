{-# LANGUAGE OverloadedStrings #-}

module Configuration.Ast where

import Language.Ast (Value(..))
import           Data.Yaml                      ( FromJSON(..))

import Data.Scientific (toRealFloat)
import qualified Data.Yaml                     as Y

instance FromJSON Value where
  parseJSON (Y.Number v) = return (Number $ toRealFloat v)
  parseJSON _            = fail "Expected Number for Config value"
