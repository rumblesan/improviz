{-# LANGUAGE TemplateHaskell #-}

module Improviz.UI where

import           Lens.Simple

data ImprovizUI = ImprovizUI
  { _displayText :: Bool
  , _currentText :: String
  }
  deriving (Eq, Show)

makeLenses ''ImprovizUI

toggleTextDisplay :: ImprovizUI -> ImprovizUI
toggleTextDisplay = over displayText not
