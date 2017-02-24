module AppTypes where

import Data.Time.Clock.POSIX (getPOSIXTime)

import Language.LanguageAst ( Block(..) )

data AppState = AppState {
  time :: Double,
  validAst :: Block,
  timeAtStart :: Double
}

makeAppState :: IO AppState
makeAppState = do
  timeNow <- realToFrac <$> getPOSIXTime
  return AppState {
    time = 0, validAst = Block [], timeAtStart = timeNow
  }
