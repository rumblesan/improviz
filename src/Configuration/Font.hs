{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Configuration.Font
  ( filepath
  , size
  , fgColour
  , bgColour
  , ImprovizFontConfig
  , defaultFontConfig
  ) where

import           Graphics.Rendering.OpenGL (Color4 (..), GLfloat)

import           Lens.Simple               (makeLenses, (^.))

import           Data.Yaml                 (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml                 as Y

data ImprovizFontConfig = ImprovizFontConfig
  { _filepath :: Maybe FilePath
  , _size     :: Int
  , _fgColour :: Color4 GLfloat
  , _bgColour :: Color4 GLfloat
  } deriving (Show)

makeLenses ''ImprovizFontConfig

defaultFontConfig =
  ImprovizFontConfig
  { _filepath = Nothing
  , _size = 36
  , _fgColour = Color4 0.0 0.0 0.0 1.0
  , _bgColour = Color4 1.0 0.8 0.0 1.0
  }

instance (Fractional a, FromJSON a) => FromJSON (Color4 a) where
  parseJSON v = do
    (r, g, b, a) <- parseJSON v
    return $ Color4 (r / 255) (g / 255) (b / 255) (a / 255)

instance FromJSON ImprovizFontConfig where
  parseJSON (Y.Object v) =
    ImprovizFontConfig <$> v .:? "filepath" <*>
    v .:? "size" .!= (defaultFontConfig ^. size) <*>
    v .:? "foregroundColour" .!= (defaultFontConfig ^. fgColour) <*>
    v .:? "backgroundColour" .!= (defaultFontConfig ^. bgColour)
  parseJSON _ = fail "Expected Object for FontConfig value"
