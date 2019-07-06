module Language.StdLib.PostEffects
  ( addPostEffectsStdLib
  )
where

import           Gfx.PostProcessing             ( AnimationStyle(..) )

import           Language.Ast                   ( Value(Symbol, Null) )
import           Language.Interpreter           ( useGfxCtx
                                                , setBuiltIn
                                                )
import           Gfx.Context                    ( setAnimationStyle )
import           Language.Interpreter.Types     ( InterpreterProcess )

addPostEffectsStdLib :: InterpreterProcess ()
addPostEffectsStdLib = do
  setBuiltIn "paintOver"      paintOver
  setBuiltIn "motionBlur"     motionBlur
  setBuiltIn "animationStyle" animationStyle

motionBlur :: [Value] -> InterpreterProcess Value
motionBlur _ = useGfxCtx (`setAnimationStyle` MotionBlur) >> return Null

paintOver :: [Value] -> InterpreterProcess Value
paintOver _ = useGfxCtx (`setAnimationStyle` PaintOver) >> return Null

animationStyle :: [Value] -> InterpreterProcess Value
animationStyle args = do
  case args of
    [Symbol "paintOver" ] -> useGfxCtx (`setAnimationStyle` PaintOver)
    [Symbol "motionBlur"] -> useGfxCtx (`setAnimationStyle` MotionBlur)
    _                     -> useGfxCtx (`setAnimationStyle` NormalStyle)
  return Null
