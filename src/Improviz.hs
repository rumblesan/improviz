{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Improviz
  ( ImprovizEnv
  , language
  , ui
  , graphics
  , config
  , startTime
  , externalVars
  , createEnv
  )
where

import           Control.Concurrent.STM         ( TVar
                                                , newTVarIO
                                                )
import           Control.Monad                  ( mapM )
import qualified Data.ByteString.Char8         as B
import           Data.Either                    ( rights )
import qualified Data.Map.Strict               as M
import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , getPOSIXTime
                                                )

import           Lens.Simple                    ( makeLenses
                                                , (^.)
                                                )

import           Configuration                  ( ImprovizConfig
                                                , codeFiles
                                                , getConfig
                                                )
import           Gfx                            ( emptyGfx )
import           Gfx.EngineState                ( EngineState )

import           Improviz.Language              ( ImprovizLanguage
                                                , makeLanguageState
                                                )
import           Improviz.UI                    ( ImprovizUI
                                                , defaultUI
                                                )
import qualified Language                      as L
import qualified Language.Ast                  as LA
import           Logging                        ( logError
                                                , logInfo
                                                )

data ImprovizEnv = ImprovizEnv
  { _language     :: TVar ImprovizLanguage
  , _ui           :: TVar ImprovizUI
  , _graphics     :: TVar EngineState
  , _config       :: ImprovizConfig
  , _startTime    :: POSIXTime
  , _externalVars :: TVar (M.Map String LA.Value)
  }

makeLenses ''ImprovizEnv

createEnv :: IO ImprovizEnv
createEnv = do
  logInfo "*****************************"
  logInfo "Creating Improviz Environment"
  startTime     <- getPOSIXTime
  uiState       <- newTVarIO defaultUI
  config        <- getConfig
  gfxState      <- newTVarIO emptyGfx
  externalVars  <- newTVarIO M.empty
  uiState       <- newTVarIO defaultUI
  userCode      <- readExternalCode (config ^. codeFiles)
  languageState <- newTVarIO (makeLanguageState userCode)
  return
    $ ImprovizEnv languageState uiState gfxState config startTime externalVars

readExternalCode :: [FilePath] -> IO [LA.Program]
readExternalCode files = rights <$> mapM readCode files
 where
  readCode path = do
    d <- B.unpack <$> B.readFile path
    let result = L.parse d
    case result of
      Right _ -> do
        logInfo $ "Loaded " ++ path ++ " user code file"
      Left err -> do
        logError $ "Could not load " ++ path ++ ": " ++ err
    return result
