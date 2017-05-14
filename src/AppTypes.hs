module AppTypes where

import Language.Ast ( Block(..) )

data AppState = AppState {
  programText :: String,
  currentAst :: Block,
  lastWorkingAst :: Block
}

makeAppState :: AppState
makeAppState = AppState {
  programText = "",
  currentAst = Block [],
  lastWorkingAst = Block []
}
