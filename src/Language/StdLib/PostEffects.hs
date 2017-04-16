module Language.StdLib.PostEffects (
  paintOver, motionBlur
) where

import Gfx.PostProcessing (AnimationStyle(..))

import Language.Interpreter.Types (InterpreterProcess)
import Language.Interpreter (setAnimationStyle)
import Language.LanguageAst (Block, Value(Null))


motionBlur :: Maybe Block -> InterpreterProcess Value
motionBlur _ = setAnimationStyle MotionBlur

paintOver :: Maybe Block -> InterpreterProcess Value
paintOver _ = setAnimationStyle PaintOver

