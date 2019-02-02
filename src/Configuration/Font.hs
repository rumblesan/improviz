{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Configuration.Font where

import           Gfx.Types                      ( Colour(..) )

import           Lens.Simple                    ( makeLenses
                                                , (^.)
                                                )

import           Data.Yaml                      ( FromJSON(..)
                                                , (.!=)
                                                , (.:?)
                                                )
import qualified Data.Yaml                     as Y

data ImprovizFontConfig = ImprovizFontConfig
  { _filepath :: Maybe FilePath
  , _size     :: Int
  , _fgColour :: Colour
  , _bgColour :: Colour
  } deriving (Show)

makeLenses ''ImprovizFontConfig

defaultFontConfig = ImprovizFontConfig { _filepath = Nothing
                                       , _size     = 36
                                       , _fgColour = Colour 0.0 0.0 0.0 1.0
                                       , _bgColour = Colour 1.0 0.8 0.0 1.0
                                       }

instance FromJSON ImprovizFontConfig where
  parseJSON (Y.Object v) =
    ImprovizFontConfig
      <$> v
      .:? "filepath"
      <*> v
      .:? "size"
      .!= (defaultFontConfig ^. size)
      <*> v
      .:? "foregroundColour"
      .!= (defaultFontConfig ^. fgColour)
      <*> v
      .:? "backgroundColour"
      .!= (defaultFontConfig ^. bgColour)
  parseJSON _ = fail "Expected Object for FontConfig value"
