module LCLangLite (
  parseLCLang,
  interpretLCLang,
  createGfx,
  module LCLangLite.LanguageAst
  ) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import LCLangLite.LanguageParser (parseProgram)
import LCLangLite.Interpreter.Types (currentGfx)
import LCLangLite.Interpreter (interpretLanguage, emptyState, setBuiltIn)
import LCLangLite.LanguageAst (Block, Value)
import qualified LCLangLite.StdLib as SL
import qualified Gfx.GfxAst as GA

parseLCLang :: String -> Maybe Block
parseLCLang program = case parseProgram program of
  Left _ -> Nothing
  Right ast -> Just ast

interpretLCLang :: Block -> (Either String Value, [String])
interpretLCLang block =
  let
     run = do
       setBuiltIn "box" SL.box ["a", "b", "c"]
       interpretLanguage block
  in
    evalState (runWriterT (runExceptT run)) emptyState

createGfx :: Block -> (Either String GA.Block, [String])
createGfx block =
  let
     run = do
       setBuiltIn "box" SL.box ["a", "b", "c"]
       _ <- interpretLanguage block
       s <- get
       return $ currentGfx s
  in
    evalState (runWriterT (runExceptT run)) emptyState
