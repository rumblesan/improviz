module Language
  ( initialInterpreterState
  , parse
  , interpret
  , setInterpreterVariables
  , updateSystemVars
  , module Language.Ast
  ) where

import           Control.Monad                  ( forM_ )
import           Control.Monad.Trans            ( liftIO )
import qualified Data.Map.Strict               as M
import           Lens.Simple                    ( set )

import           Gfx.Context                    ( GfxContext )

import           Language.Ast                   ( Identifier
                                                , Program
                                                , Value(..)
                                                )
import           Language.Ast.Transformers      ( transform )
import           Language.Interpreter           ( interpretLanguage )
import           Language.Interpreter.StdLib    ( addStdLib )
import qualified Language.Interpreter.Types    as LT
import           Language.Interpreter.Types     ( InterpreterState
                                                , externals
                                                , getGlobalNames
                                                , gfxContext
                                                , runInterpreterM
                                                , setGlobal
                                                , setSystemVars
                                                , systemVars
                                                )
import           Language.Parser                ( parseProgram )
import           Language.Parser.Errors         ( ParserError )
import           Logging                        ( logInfo )


parse :: String -> Either ParserError Program
parse = parseProgram

initialInterpreterState
  :: [(Identifier, Value)]
  -> [(FilePath, Program)]
  -> GfxContext
  -> IO InterpreterState
initialInterpreterState systemVariables userCode ctx =
  let langState = set gfxContext ctx LT.empty
      setup     = do
        setSystemVars systemVariables
        addStdLib
        globals <- getGlobalNames
        mapM (load globals) userCode
  in  snd <$> runInterpreterM setup langState
 where
  load globals (fp, code) = do
    liftIO $ logInfo ("Loading " ++ fp)
    interpretLanguage $ transform globals code

updateSystemVars
  :: [(Identifier, Value)] -> InterpreterState -> InterpreterState
updateSystemVars newSysVars = set systemVars (M.fromList newSysVars)

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
