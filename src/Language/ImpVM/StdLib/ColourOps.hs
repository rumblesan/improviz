module Language.ImpVM.StdLib.ColourOps
  ( colourBuiltIns
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Lens.Simple                    ( use )

import           Language.ImpVM.Types           ( VM
                                                , StackItem(..)
                                                , externalState
                                                )
import           Language.ImpVM.VM              ( setError )

import           Gfx.Context                    ( GfxContext(..) )

dToC :: Float -> Float
dToC c = max 0 (min c 255) / 255

colourBuiltIns :: [(String, [StackItem] -> VM GfxContext ())]
colourBuiltIns = [("background", background), ("style", style)]

background :: [StackItem] -> VM GfxContext ()
background args = do
  (rVal, gVal, bVal) <- case args of
    []                   -> return (255, 255, 255)
    [SFloat x]           -> return (x, x, x)
    [SFloat x, SFloat y] -> return (x, y, 0)
    SFloat x : SFloat y : SFloat z : _ -> return (x, y, z)
  ctx <- use externalState
  liftIO $ setBackground ctx (dToC rVal) (dToC gVal) (dToC bVal)

style :: [StackItem] -> VM GfxContext ()
style styleArgs = do
  ctx <- use externalState
  case styleArgs of
    SString "fill"     : rest -> runFill rest
    SString "noFill"   : _    -> liftIO $ noFill ctx
    SString "stroke"   : rest -> runStroke rest
    SString "noStroke" : _    -> liftIO $ noStroke ctx
    SString "texture"  : rest -> runTexture rest
 where
  runFill args = case args of
    [SFloat r, SFloat g, SFloat b, SFloat a] -> do
      ctx <- use externalState
      liftIO $ colourFill ctx (dToC r) (dToC g) (dToC b) (dToC a)
    _ -> setError "Error with functions to fill"
  runStroke args = case args of
    [SFloat r, SFloat g, SFloat b, SFloat a] -> do
      ctx <- use externalState
      liftIO $ colourStroke ctx (dToC r) (dToC g) (dToC b) (dToC a)
    _ -> setError "Error with functions to fill"
  runTexture args = case args of
    SString name : SFloat frame : _ -> do
      ctx <- use externalState
      liftIO $ textureFill ctx name frame
    SString name : _ -> do
      ctx <- use externalState
      liftIO $ textureFill ctx name 0
    _ -> setError "Error with functions to texture"
