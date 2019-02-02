{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Configuration.Screen where

import           Lens.Simple                    ( makeLenses
                                                , (^.)
                                                )

import           Data.Yaml                      ( FromJSON(..)
                                                , (.!=)
                                                , (.:?)
                                                )
import qualified Data.Yaml                     as Y

data ImprovizScreenConfig = ImprovizScreenConfig
  { _front :: Float
  , _back  :: Float
  } deriving (Show)

makeLenses ''ImprovizScreenConfig

defaultScreenConfig = ImprovizScreenConfig { _front = 0.1, _back = 100 }

instance FromJSON ImprovizScreenConfig where
  parseJSON (Y.Object v) =
    ImprovizScreenConfig
      <$> v
      .:? "front"
      .!= (defaultScreenConfig ^. front)
      <*> v
      .:? "back"
      .!= (defaultScreenConfig ^. back)
  parseJSON _ = fail "Expected Object for ScreenConfig value"
