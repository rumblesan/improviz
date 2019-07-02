module Language
  ( initialState
  , parse
  , simpleParse
  , interpret
  , updateStateVariables
  , setGfxEngine
  , getGfxEngine
  , module Language.Ast
  )
where

import           Control.Monad                  ( forM_ )
import           Lens.Simple                    ( set
                                                , view
                                                )

import           Gfx                            ( EngineState )

import           Language.Ast                   ( Identifier
                                                , Program
                                                , Value(..)
                                                )
import           Language.Ast.Transformers      ( transform )
import           Language.Interpreter           ( emptyState
                                                , getGlobalNames
                                                , interpretLanguage
                                                , setVariable
                                                )
import           Language.Interpreter.Types     ( InterpreterState
                                                , gfxEngine
                                                , runInterpreterM
                                                )
import           Language.Parser                ( parseProgram )
import           Language.Parser.Errors         ( ParserError )
import           Language.StdLib                ( addStdLib )

parse :: String -> Either ParserError Program
parse = parseProgram

simpleParse :: String -> Either String Program
simpleParse code = case parseProgram code of
  Left  err -> Left $ show err
  Right ast -> Right ast

initialState :: [Program] -> IO InterpreterState
initialState userCode =
  let setup = do
        addStdLib
        globals <- getGlobalNames
        mapM (interpretLanguage . transform globals) userCode
  in  snd <$> runInterpreterM setup emptyState

updateStateVariables
  :: [(Identifier, Value)] -> InterpreterState -> IO InterpreterState
updateStateVariables vars oldState =
  let setVars = forM_ vars (uncurry setVariable)
  in  snd <$> runInterpreterM setVars oldState

setGfxEngine :: EngineState -> InterpreterState -> InterpreterState
setGfxEngine es = set gfxEngine (Just es)

getGfxEngine :: InterpreterState -> Maybe EngineState
getGfxEngine = view gfxEngine

interpret
  :: InterpreterState -> Program -> IO (Either String Value, InterpreterState)
interpret initialState program =
  let run = do
        globals <- getGlobalNames
        interpretLanguage (transform globals program)
  in  runInterpreterM run initialState
