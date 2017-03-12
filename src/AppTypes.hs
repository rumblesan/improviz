module AppTypes where

import Language.LanguageAst ( Block(..) )

data AppState = AppState { validAst :: Block }

makeAppState :: AppState
makeAppState = AppState { validAst = Block [] }
