module Configuration
  ( ImpConfig(..)
  , getCliArgs
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data ImpConfig = ImpConfig
  { screenWidth       :: Int
  , screenHeight      :: Int
  , fullscreenDisplay :: Maybe Int
  }

cliparser :: Parser ImpConfig
cliparser =
  ImpConfig <$>
  option
    auto
    (long "width" <> short 'w' <> showDefault <> value 640 <>
     help "Screen width" <>
     metavar "INT") <*>
  option
    auto
    (long "height" <> short 'h' <> showDefault <> value 480 <>
     help "Screen height" <>
     metavar "INT") <*>
  optional
    (option
       auto
       (long "fullscreen" <> short 'f' <>
        help "Which screen to fullscreen the app to" <>
        metavar "INT"))

getCliArgs :: (ImpConfig -> IO ()) -> IO ()
getCliArgs app = app =<< execParser opts
  where
    opts =
      info
        (cliparser <**> helper)
        (fullDesc <> progDesc "Visual live coding environment" <>
         header "Improviz")
