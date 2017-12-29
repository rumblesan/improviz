module Configuration.CLIConfig where

import           Options.Applicative

import           Data.Semigroup      ((<>))

data ImpCLIConfig = ImpCLIConfig
  { cliScreenWidth       :: Maybe Int
  , cliScreenHeight      :: Maybe Int
  , cliFullscreenDisplay :: Maybe Int
  , cliDebug             :: Bool
  , cliConfigFilePath    :: Maybe FilePath
  }

cliopts :: ParserInfo ImpCLIConfig
cliopts =
  info
    (cliparser <**> helper)
    (fullDesc <> progDesc "Visual live coding environment" <> header "Improviz")

cliparser :: Parser ImpCLIConfig
cliparser =
  ImpCLIConfig <$>
  optional
    (option
       auto
       (long "width" <> short 'w' <> help "Screen width" <> metavar "INT")) <*>
  optional
    (option
       auto
       (long "height" <> short 'h' <> help "Screen height" <> metavar "INT")) <*>
  optional
    (option
       auto
       (long "fullscreen" <> short 'f' <>
        help "Which screen to fullscreen the app to" <>
        metavar "INT")) <*>
  switch (long "debug" <> short 'd' <> help "Put improviz in debug mode") <*>
  optional
    (option
       str
       (long "config" <> short 'c' <> help "Path to a configuration YAML file" <>
        metavar "FilePath"))
