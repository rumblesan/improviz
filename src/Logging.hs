module Logging
  ( logInfo
  , logDebug
  , logError
  )
where

import           Control.Monad                  ( when )
import           Data.Monoid                    ( (<>) )
import           Data.Time
import           Lens.Simple                    ( (^.) )

import qualified Configuration                 as C

getTime :: IO String
getTime = formatTime defaultTimeLocale "%F %H:%M:%6Q" <$> getCurrentTime

logInfo :: String -> IO ()
logInfo message = do
  t <- getTime
  putStrLn $ t <> "  INFO: " <> message

logDebug :: C.ImprovizConfig -> String -> IO ()
logDebug config message = when (config ^. C.debug) $ do
  t <- getTime
  putStrLn $ t <> " DEBUG: " <> message

logError :: String -> IO ()
logError message = do
  t <- getTime
  putStrLn $ t <> " ERROR: " <> message
