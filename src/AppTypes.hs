module AppTypes where

import Language.LanguageAst ( Block(..) )

data AppState = AppState {
  currentAst :: Block,
  lastWorkingAst :: Block
}

makeAppState :: AppState
makeAppState = AppState {
  currentAst = Block [],
  lastWorkingAst = Block []
}
