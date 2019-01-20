module Gfx.Types where

import           Data.Yaml (FromJSON (..))

data Colour =
  Colour Float
         Float
         Float
         Float
  deriving (Eq, Show)

instance FromJSON Colour where
  parseJSON v = do
    (r, g, b, a) <- parseJSON v
    return $ Colour (r / 255) (g / 255) (b / 255) (a / 255)
