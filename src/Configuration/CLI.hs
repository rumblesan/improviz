{-# LANGUAGE TemplateHaskell #-}

module Configuration.CLI
  ( screenWidth
  , screenHeight
  , fullscreenDisplay
  , debug
  , configFilePath
  , ImprovizCLIConfig
  , opts
  ) where

import           Data.Semigroup                 ( (<>) )
import           Options.Applicative

import           Lens.Simple                    ( makeLenses )

data ImprovizCLIConfig = ImprovizCLIConfig
  { _screenWidth       :: Maybe Int
  , _screenHeight      :: Maybe Int
  , _fullscreenDisplay :: Maybe Int
  , _debug             :: Bool
  , _configFilePath    :: Maybe FilePath
  }
  deriving Show

makeLenses ''ImprovizCLIConfig

opts :: ParserInfo ImprovizCLIConfig
opts = info
  (parser <**> helper)
  (fullDesc <> progDesc "Visual live coding environment" <> header "Improviz")

parser :: Parser ImprovizCLIConfig
parser =
  ImprovizCLIConfig
    <$> optional
          (option
            auto
            (long "width" <> short 'w' <> help "Screen width" <> metavar "INT")
          )
    <*> optional
          (option
            auto
            (long "height" <> short 'h' <> help "Screen height" <> metavar "INT"
            )
          )
    <*> optional
          (option
            auto
            (  long "fullscreen"
            <> short 'f'
            <> help "Which screen to fullscreen the app to"
            <> metavar "INT"
            )
          )
    <*> switch (long "debug" <> short 'd' <> help "Put improviz in debug mode")
    <*> optional
          (option
            str
            (  long "config"
            <> short 'c'
            <> help "Path to a configuration YAML file"
            <> metavar "FilePath"
            )
          )
