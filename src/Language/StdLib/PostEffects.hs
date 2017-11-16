module Language.StdLib.PostEffects
  ( paintOver
  , motionBlur
  ) where

import           Gfx.PostProcessing         (AnimationStyle (..))

import           Language.Ast               (Block, Value (Null))
import           Language.Interpreter       (setAnimationStyle)
import           Language.Interpreter.Types (InterpreterProcess)

motionBlur :: InterpreterProcess Value
motionBlur = setAnimationStyle MotionBlur

paintOver :: InterpreterProcess Value
paintOver = setAnimationStyle PaintOver
