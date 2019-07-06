module TestHelpers.Util
  ( gfxTest
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

import           Gfx.Context                    ( emptyGfxContext )
import qualified Gfx.Ast                       as GA
import qualified Language                      as L
import           Language.Ast
import           Language.Parser.Errors         ( prettyPrintErrors )

gfxTest :: String -> [GA.GfxCommand] -> Assertion
gfxTest program expectedGfx = case L.parse program of
  Left  err -> assertFailure $ prettyPrintErrors err
  Right ast -> do
    (out, ctx)       <- createGfxContextHelpers
    interpreterState <- L.initialState [] ctx
    result           <- fst <$> L.interpret interpreterState ast
    assertEqual "interpreter runs" (Right Null) result
    gfx <- getOutputGfx out
    assertEqual "correct GFX" gfx expectedGfx

resultTest :: String -> Value -> String -> Assertion
resultTest program expected message = case L.parse program of
  Left  err -> assertFailure $ prettyPrintErrors err
  Right ast -> do
    interpreterState <- L.initialState [] emptyGfxContext
    result           <- fst <$> L.interpret interpreterState ast
    assertEqual message (Right expected) result
