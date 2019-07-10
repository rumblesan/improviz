module TestHelpers.Util
  ( gfxTest
  , resultTest
  , vmGfxTest
  , vmResultTest
  )
where

import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S

import           Lens.Simple                    ( view )

import           Test.HUnit                     ( Assertion
                                                , assertEqual
                                                , assertFailure
                                                )
import           TestHelpers.GfxContext         ( createGfxContextHelpers
                                                , getOutputGfx
                                                )

import           Gfx.Context                    ( GfxContext
                                                , emptyGfxContext
                                                )
import qualified Gfx.Ast                       as GA
import qualified Language                      as L
import           Language.Ast
import           Language.Parser.Errors         ( prettyPrintErrors )

import           Language.ImpVM                 ( run )
import           Language.ImpVM.StdLib          ( builtInFuncs )
import           Language.ImpVM.Types           ( VMState
                                                , StackItem
                                                , vmError
                                                )
import qualified Language.Compiler             as C
import           Language.Ast.Transformers      ( transform )

gfxTest :: String -> [GA.GfxCommand] -> Assertion
gfxTest program expectedGfx = case L.parse program of
  Left  err -> assertFailure $ prettyPrintErrors err
  Right ast -> do
    (out, ctx)       <- createGfxContextHelpers
    interpreterState <- L.initialInterpreterState [] ctx
    result           <- fst <$> L.interpret interpreterState ast
    assertEqual "interpreter runs" (Right Null) result
    gfx <- getOutputGfx out
    assertEqual "correct GFX" gfx expectedGfx

resultTest :: String -> Value -> String -> Assertion
resultTest program expected message = case L.parse program of
  Left  err -> assertFailure $ prettyPrintErrors err
  Right ast -> do
    interpreterState <- L.initialInterpreterState [] emptyGfxContext
    result           <- fst <$> L.interpret interpreterState ast
    assertEqual message (Right expected) result

vmGfxTest :: String -> M.Map String StackItem -> [GA.GfxCommand] -> Assertion
vmGfxTest program extVars expectedGfx =
  let builtInNames = S.fromList $ M.keys builtInFuncs
      (Right ast)  = transform builtInNames <$> L.parse program
  in  case C.compile ast of
        Left  err -> assertFailure err
        Right ops -> do
          print ast
          print ops
          (out, ctx) <- createGfxContextHelpers
          vmState    <- run ctx extVars ops
          assertEqual "No VM Error" (view vmError vmState) Nothing
          gfx <- getOutputGfx out
          assertEqual "correct GFX" gfx expectedGfx

vmResultTest
  :: String
  -> M.Map String StackItem
  -> (VMState GfxContext -> Assertion)
  -> Assertion
vmResultTest program extVars check =
  let builtInNames = S.fromList $ M.keys builtInFuncs
      (Right ast)  = transform builtInNames <$> L.parse program
  in  case C.compile ast of
        Left  err -> assertFailure err
        Right ops -> do
          vmState <- run emptyGfxContext extVars ops
          check vmState
