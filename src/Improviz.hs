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
                                                , TMVar
                                                , newEmptyTMVarIO
                                                )
import           Control.Monad                  ( mapM )
import qualified Data.ByteString.Char8         as B
import           Data.Either                    ( rights )
import qualified Data.Map.Strict               as M
import           Data.Monoid                    ( (<>) )
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
import           Gfx.EngineState                ( EngineState )

import           Improviz.Language              ( ImprovizLanguage
                                                , makeLanguageState
                                                )
import           Improviz.UI                    ( ImprovizUI
                                                , defaultUI
                                                )
import qualified Language                      as L
import qualified Language.Ast                  as LA
import           Language.Parser.Errors         ( prettyPrintErrors )
import           Logging                        ( logError
                                                , logInfo
                                                )

data ImprovizEnv = ImprovizEnv
  { _language     :: TVar ImprovizLanguage
  , _ui           :: TVar ImprovizUI
  , _graphics     :: TMVar EngineState
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
  gfxState      <- newEmptyTMVarIO
  externalVars  <- newTVarIO M.empty
  uiState       <- newTVarIO defaultUI
  userCode      <- readExternalCode (config ^. codeFiles)
  languageState <- makeLanguageState userCode >>= newTVarIO
  return
    $ ImprovizEnv languageState uiState gfxState config startTime externalVars

readExternalCode :: [FilePath] -> IO [LA.Program]
readExternalCode files = rights <$> mapM readCode files
 where
  readCode path = do
    d <- B.unpack <$> B.readFile path
    let result = L.parse d
    case result of
      Right _ -> logInfo $ "Loaded " <> path <> " user code file"
      Left err ->
        logError $ "Could not load " <> path <> "\n" <> prettyPrintErrors err
    return result
