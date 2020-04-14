module TestHelpers.Util
  ( parserTest
  , gfxTest
  , resultTest
  )
where

import           Test.HUnit                     ( Assertion
                                                , assertEqual
                                                , assertFailure
                                                )
import           TestHelpers.GfxContext         ( createGfxContextHelpers
                                                , getOutputGfx
                                                )
import qualified TestHelpers.GfxAst            as GA

import qualified Gfx.Context                   as GfxC
import qualified Language                      as L
import           Language.Ast
import           Language.Parser.Errors         ( prettyPrintErrors )

parserTest :: String -> Program -> Assertion
parserTest program expectedAst = case L.parse program of
  Left  err -> assertFailure $ prettyPrintErrors err
  Right ast -> assertEqual "correct AST" ast expectedAst

gfxTest :: String -> [GA.GfxCommand] -> Assertion
gfxTest program expectedGfx = case L.parse program of
  Left  err -> assertFailure $ prettyPrintErrors err
  Right ast -> do
    (out, ctx)       <- createGfxContextHelpers
    interpreterState <- L.initialInterpreterState [] [] ctx
    result           <- fst <$> L.interpret interpreterState ast
    assertEqual "interpreter runs" (Right Null) result
    gfx <- getOutputGfx out
    assertEqual "correct GFX" gfx expectedGfx

resultTest :: String -> Value -> String -> Assertion
resultTest program expected message = case L.parse program of
  Left  err -> assertFailure $ prettyPrintErrors err
  Right ast -> do
    interpreterState <- L.initialInterpreterState [] [] GfxC.empty
    result           <- fst <$> L.interpret interpreterState ast
    assertEqual message (Right expected) result
