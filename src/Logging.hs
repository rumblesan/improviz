module Logging where

import           Data.Monoid ((<>))
import           Data.Time

getTime :: IO String
getTime = show <$> getCurrentTime

logInfo :: String -> IO ()
logInfo message = do
  t <- getTime
  putStrLn $ t <> "  INFO: " <> message

logError :: String -> IO ()
logError message = do
  t <- getTime
  putStrLn $ t <> " ERROR: " <> message
