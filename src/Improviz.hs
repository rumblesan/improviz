{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Improviz
  ( ImprovizEnv
  , runtime
  , ui
  , graphics
  , config
  , gfxContext
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
import           Data.Monoid                    ( (<>) )
import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , getPOSIXTime
                                                )

import           Lens.Simple                    ( makeLenses
                                                , (^.)
                                                )

import           Configuration                  ( ImprovizConfig
                                                , codeFiles
                                                , showText
                                                )
import           Gfx.Engine                     ( GfxEngine )
import           Gfx.Context                    ( GfxContext
                                                , createGfxContext
                                                )

import           Improviz.Runtime               ( ImprovizRuntime
                                                , makeRuntimeState
                                                )
import           Improviz.UI                    ( ImprovizUI(..) )
import qualified Language                      as L
import qualified Language.Ast                  as LA
import           Language.Parser.Errors         ( prettyPrintErrors )
import           Logging                        ( logError
                                                , logInfo
                                                )

data ImprovizEnv = ImprovizEnv
  { _runtime      :: TVar (ImprovizRuntime GfxContext)
  , _ui           :: TVar ImprovizUI
  , _graphics     :: TVar GfxEngine
  , _gfxContext   :: GfxContext
  , _config       :: ImprovizConfig
  , _startTime    :: POSIXTime
  , _externalVars :: TVar (M.Map String LA.Value)
  }

makeLenses ''ImprovizEnv

createEnv :: ImprovizConfig -> GfxEngine -> IO ImprovizEnv
createEnv config gfx = do
  logInfo "*****************************"
  logInfo "Creating Improviz Environment"
  startTime <- getPOSIXTime
  gfxState  <- newTVarIO gfx
  let gfxContext = createGfxContext gfxState
  externalVars <- newTVarIO M.empty
  uiState      <- newTVarIO $ ImprovizUI (config ^. showText) ""
  userCode     <- readExternalCode (config ^. codeFiles)
  runtimeState <- makeRuntimeState userCode gfx gfxContext >>= newTVarIO
  return $ ImprovizEnv runtimeState
                       uiState
                       gfxState
                       gfxContext
                       config
                       startTime
                       externalVars

readExternalCode :: [FilePath] -> IO [(FilePath, LA.Program)]
readExternalCode files = rights <$> mapM readCode files
 where
  readCode path = do
    d <- B.unpack <$> B.readFile path
    let result = L.parse d
    case result of
      Right prog -> return $ Right (path, prog)
      Left  err  -> do
        logError $ "Could not load " <> path <> "\n" <> prettyPrintErrors err
        return $ Left "error"
