module AppTypes where

import qualified Language.LanguageAst as LA

data AppState = AppState {
  time :: Double,
  validAst :: LA.Block,
  timeAtStart :: Double
}
