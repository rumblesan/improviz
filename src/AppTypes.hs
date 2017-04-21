module AppTypes where

import Language.Ast ( Block(..) )

data AppState = AppState {
  currentAst :: Block,
  lastWorkingAst :: Block
}

makeAppState :: AppState
makeAppState = AppState {
  currentAst = Block [],
  lastWorkingAst = Block []
}
