module AppTypes where

import Language.Ast ( Block(..) )

data AppState = AppState {
  displayText :: Bool,
  programText :: String,
  currentAst :: Block,
  lastWorkingAst :: Block
}

makeAppState :: AppState
makeAppState = AppState {
  displayText = True,
  programText = "",
  currentAst = Block [],
  lastWorkingAst = Block []
}
