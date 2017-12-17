module Language.StdLib.PostEffects
  ( addPostEffectsStdLib
  ) where

import           Gfx.PostProcessing         (AnimationStyle (..))

import           Language.Ast               (Block, Value (Null))
import           Language.Interpreter       (setAnimationStyle, setBuiltIn)
import           Language.Interpreter.Types (InterpreterProcess)

addPostEffectsStdLib :: InterpreterProcess ()
addPostEffectsStdLib = do
  setBuiltIn "paintOver" paintOver []
  setBuiltIn "motionBlur" motionBlur []

motionBlur :: InterpreterProcess Value
motionBlur = setAnimationStyle MotionBlur

paintOver :: InterpreterProcess Value
paintOver = setAnimationStyle PaintOver
