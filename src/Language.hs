module Language
  ( initialInterpreterState
  , parse
  , interpret
  , setInterpreterVariables
  , module Language.Ast
  )
where

import qualified Data.Map.Strict               as M
import           Control.Monad                  ( forM_ )
import           Lens.Simple                    ( set )

import           Gfx.Context                    ( GfxContext )

import           Language.Ast                   ( Identifier
                                                , Program
                                                , Value(..)
                                                )
import           Language.Ast.Transformers      ( transform )
import           Language.Interpreter           ( emptyState
                                                , getGlobalNames
                                                , interpretLanguage
                                                , setGlobal
                                                )
import           Language.Interpreter.Types     ( InterpreterState
                                                , gfxContext
                                                , runInterpreterM
                                                , externals
                                                )
import           Language.Parser                ( parseProgram )
import           Language.Parser.Errors         ( ParserError )
import           Language.StdLib                ( addStdLib )


parse :: String -> Either ParserError Program
parse = parseProgram

initialInterpreterState :: [Program] -> GfxContext -> IO InterpreterState
initialInterpreterState userCode ctx =
  let langState = set gfxContext ctx emptyState
      setup     = do
        addStdLib
        globals <- getGlobalNames
        mapM (interpretLanguage . transform globals) userCode
  in  snd <$> runInterpreterM setup langState

setInterpreterVariables
  :: [(Identifier, Value)]
  -> M.Map String Value
  -> InterpreterState
  -> IO InterpreterState
setInterpreterVariables globals externalVars is =
  let setVars = forM_ globals (uncurry setGlobal)
  in  do
        (_, newState) <- runInterpreterM setVars is
        return $ set externals externalVars newState

interpret
  :: InterpreterState -> Program -> IO (Either String Value, InterpreterState)
interpret initialState program =
  let run = do
        globals <- getGlobalNames
        interpretLanguage (transform globals program)
  in  runInterpreterM run initialState
