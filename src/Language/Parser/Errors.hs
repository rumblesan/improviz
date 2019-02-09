{-# LANGUAGE DeriveGeneric #-}

module Language.Parser.Errors where

import           Control.Monad                  ( mapM )
import           Control.Monad.State            ( State(..)
                                                , gets
                                                , put
                                                , evalState
                                                )
import           GHC.Generics
import           Data.Void
import qualified Data.List.NonEmpty            as NE

import           Data.Aeson

import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Pos            ( unPos
                                                , SourcePos(..)
                                                )
import           Text.Megaparsec.Error          ( parseErrorTextPretty
                                                , errorOffset
                                                )
import           Text.Megaparsec.Stream         ( reachOffsetNoLine )


type ParserError = ParseErrorBundle String Void

data ImprovizCodeError = ImprovizCodeError
  { line :: Int
  , column :: Int
  , message :: String
  } deriving (Generic, Show, Eq)

instance ToJSON ImprovizCodeError where
  toEncoding = genericToEncoding defaultOptions

parseErrorToIError
  :: ParseError String Void -> State (PosState String) ImprovizCodeError
parseErrorToIError e = do
  (epos, pst') <- gets (reachOffsetNoLine (errorOffset e))
  put pst'
  let lineNum = unPos $ sourceLine epos
  let colNum  = unPos $ sourceColumn epos
  let msg     = parseErrorTextPretty e
  return $ ImprovizCodeError lineNum colNum msg

parseErrorsOut :: ParseErrorBundle String Void -> [ImprovizCodeError]
parseErrorsOut pBundle =
  let errors = mapM parseErrorToIError (bundleErrors pBundle)
  in  NE.toList $ evalState errors (bundlePosState pBundle)

prettyPrintErrors :: ParseErrorBundle String Void -> String
prettyPrintErrors = errorBundlePretty
