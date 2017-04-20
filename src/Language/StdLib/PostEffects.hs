module Language.StdLib.PostEffects (
  paintOver, motionBlur
) where

import Gfx.PostProcessing (AnimationStyle(..))

import Language.Interpreter.Types (InterpreterProcess)
import Language.Interpreter (setAnimationStyle)
import Language.LanguageAst (Block, Value(Null))


motionBlur :: InterpreterProcess Value
motionBlur = setAnimationStyle MotionBlur

paintOver :: InterpreterProcess Value
paintOver = setAnimationStyle PaintOver

