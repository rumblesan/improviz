module Language.StdLib.PostEffects
  ( addPostEffectsStdLib
  )
where

import           Gfx.PostProcessing             ( AnimationStyle(..) )

import           Language.Ast                   ( Block
                                                , Value(Null)
                                                )
import           Language.Interpreter           ( setAnimationStyle
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )

addPostEffectsStdLib :: InterpreterProcess ()
addPostEffectsStdLib = do
  setBuiltIn "paintOver"  paintOver
  setBuiltIn "motionBlur" motionBlur

motionBlur :: [Value] -> Maybe Block -> InterpreterProcess Value
motionBlur _ _ = setAnimationStyle MotionBlur >> return Null

paintOver :: [Value] -> Maybe Block -> InterpreterProcess Value
paintOver _ _ = setAnimationStyle PaintOver >> return Null
