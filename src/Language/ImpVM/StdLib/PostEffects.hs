module Language.ImpVM.StdLib.PostEffects
  ( postEffectsBuiltIns
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Lens.Simple                    ( use )

import           Language.ImpVM.Types           ( VM
                                                , StackItem(..)
                                                , externalState
                                                )
import           Gfx.Context                    ( GfxContext(..) )
import           Gfx.PostProcessing             ( AnimationStyle(..) )

postEffectsBuiltIns :: [(String, [StackItem] -> VM GfxContext ())]
postEffectsBuiltIns =
  [ ("paintOver"     , paintOver)
  , ("motionBlur"    , motionBlur)
  , ("animationStyle", animationStyle)
  ]

motionBlur :: [StackItem] -> VM GfxContext ()
motionBlur _ = do
  ctx <- use externalState
  liftIO $ setAnimationStyle ctx MotionBlur

paintOver :: [StackItem] -> VM GfxContext ()
paintOver _ = do
  ctx <- use externalState
  liftIO $ setAnimationStyle ctx PaintOver

animationStyle :: [StackItem] -> VM GfxContext ()
animationStyle args = do
  ctx <- use externalState
  liftIO $ case args of
    [SString "paintOver" ] -> setAnimationStyle ctx PaintOver
    [SString "motionBlur"] -> setAnimationStyle ctx MotionBlur
    _                      -> setAnimationStyle ctx NormalStyle
