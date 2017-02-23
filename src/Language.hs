module Language (
  parse,
  interpret,
  createGfx,
  module Language.LanguageAst
  ) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

import Language.LanguageParser (parseProgram)
import Language.Interpreter.Types (currentGfx)
import Language.Interpreter (interpretLanguage, emptyState, setBuiltIn)
import Language.LanguageAst (Block, Value)
import qualified Language.StdLib as SL
import qualified Gfx.GfxAst as GA

parse :: String -> Maybe Block
parse program = case parseProgram program of
  Left _ -> Nothing
  Right ast -> Just ast

interpret :: Block -> (Either String Value, [String])
interpret block =
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
