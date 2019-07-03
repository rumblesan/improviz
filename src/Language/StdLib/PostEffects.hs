module Language.StdLib.PostEffects
  ( addPostEffectsStdLib
  )
where

import           Gfx.PostProcessing             ( AnimationStyle(..) )

import           Language.Ast                   ( Value(Symbol, Null) )
import           Language.Interpreter           ( execGfx
                                                , setBuiltIn
                                                )
import           Gfx.Commands                   ( setAnimationStyle )
import           Language.Interpreter.Types     ( InterpreterProcess )

addPostEffectsStdLib :: InterpreterProcess ()
addPostEffectsStdLib = do
  setBuiltIn "paintOver"      paintOver
  setBuiltIn "motionBlur"     motionBlur
  setBuiltIn "animationStyle" animationStyle

motionBlur :: [Value] -> InterpreterProcess Value
motionBlur _ = execGfx (setAnimationStyle MotionBlur) >> return Null

paintOver :: [Value] -> InterpreterProcess Value
paintOver _ = execGfx (setAnimationStyle PaintOver) >> return Null

animationStyle :: [Value] -> InterpreterProcess Value
animationStyle args = do
  case args of
    [Symbol "paintOver" ] -> execGfx $ setAnimationStyle PaintOver
    [Symbol "motionBlur"] -> execGfx $ setAnimationStyle MotionBlur
    _                     -> execGfx $ setAnimationStyle NormalStyle
  return Null
