module Language
  ( initialState
  , parse
  , simpleParse
  , interpret
  , createGfxScene
  , updateStateVariables
  , setGfxEngine
  , getGfxEngine
  , module Language.Ast
  )
where

import           Control.Monad                  ( forM_ )
import           Lens.Simple                    ( use )

import           Gfx                            ( EngineState
                                                , Scene(..)
                                                )

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
import           Language.Interpreter.Types     ( InterpreterProcess
                                                , InterpreterState(..)
                                                , currentGfx
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

initialState :: [Program] -> InterpreterState
initialState userCode =
  let setup = do
        addStdLib
        globals <- getGlobalNames
        mapM (interpretLanguage . transform globals) userCode
  in  snd $ runInterpreterM setup emptyState

updateStateVariables
  :: [(Identifier, Value)] -> InterpreterState -> InterpreterState
updateStateVariables vars oldState =
  let setVars = addInitialVariables vars
  in  snd $ runInterpreterM setVars oldState

setGfxEngine :: EngineState -> InterpreterState -> InterpreterState
setGfxEngine es is = is { _gfxEngine = Just es }

getGfxEngine :: InterpreterState -> Maybe EngineState
getGfxEngine = _gfxEngine

interpret :: [(Identifier, Value)] -> Program -> Either String Value
interpret initialVars program =
  let run = do
        addStdLib
        addInitialVariables initialVars
        globals <- getGlobalNames
        interpretLanguage (transform globals program)
  in  fst $ runInterpreterM run emptyState

createGfxScene
  :: InterpreterState -> Program -> (Either String Scene, InterpreterState)
createGfxScene initialState program =
  let run = do
        globals <- getGlobalNames
        _       <- interpretLanguage (transform globals program)
        gfxB    <- use currentGfx
        return Scene { sceneGfx = gfxB }
  in  runInterpreterM run initialState

addInitialVariables :: [(Identifier, Value)] -> InterpreterProcess ()
addInitialVariables vars = forM_ vars (uncurry setVariable)
