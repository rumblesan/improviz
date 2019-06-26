module Language.StdLib.PostEffects
  ( addPostEffectsStdLib
  )
where

import           Gfx.PostProcessing             ( AnimationStyle(..) )

import           Language.Ast                   ( Block
                                                , Value(Symbol, Null)
                                                )
import           Language.Interpreter           ( setAnimationStyle
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )

addPostEffectsStdLib :: InterpreterProcess ()
addPostEffectsStdLib = do
  setBuiltIn "paintOver"      paintOver
  setBuiltIn "motionBlur"     motionBlur
  setBuiltIn "animationStyle" animationStyle

motionBlur :: [Value] -> Maybe Block -> InterpreterProcess Value
motionBlur _ _ = setAnimationStyle MotionBlur >> return Null

paintOver :: [Value] -> Maybe Block -> InterpreterProcess Value
paintOver _ _ = setAnimationStyle PaintOver >> return Null

animationStyle :: [Value] -> Maybe Block -> InterpreterProcess Value
animationStyle args _ = do
  case args of
    [Symbol "paintOver" ] -> setAnimationStyle PaintOver
    [Symbol "motionBlur"] -> setAnimationStyle MotionBlur
    _                     -> setAnimationStyle NormalStyle
  return Null
