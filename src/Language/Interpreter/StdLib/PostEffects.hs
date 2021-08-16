module Language.Interpreter.StdLib.PostEffects
  ( addPostEffectsStdLib
  ) where

import           Control.Monad.Except

import           Gfx.PostProcessing             ( AnimationStyle(..) )

import           Gfx.Context                    ( setAnimationStyle
                                                , setFilterVar
                                                )
import           Language.Ast                   ( Value(Null, Symbol) )
import           Language.Interpreter.Types     ( InterpreterProcess
                                                , setBuiltIn
                                                , withGfxCtx
                                                )

addPostEffectsStdLib :: InterpreterProcess ()
addPostEffectsStdLib = do
  setBuiltIn "paintOver"      paintOver
  setBuiltIn "motionBlur"     motionBlur
  setBuiltIn "animationStyle" animationStyle
  setBuiltIn "postProcess"    internalPostProcess

motionBlur :: [Value] -> InterpreterProcess Value
motionBlur _ = withGfxCtx (`setAnimationStyle` MotionBlur) >> return Null

paintOver :: [Value] -> InterpreterProcess Value
paintOver _ = withGfxCtx (`setAnimationStyle` PaintOver) >> return Null

animationStyle :: [Value] -> InterpreterProcess Value
animationStyle args = do
  case args of
    [Symbol "paintOver" ] -> withGfxCtx (`setAnimationStyle` PaintOver)
    [Symbol "motionBlur"] -> withGfxCtx (`setAnimationStyle` MotionBlur)
    [Symbol name        ] -> withGfxCtx (`setAnimationStyle` (UserFilter name))
    _                     -> withGfxCtx (`setAnimationStyle` NormalStyle)
  return Null

internalPostProcess :: [Value] -> InterpreterProcess Value
internalPostProcess args = do
  cmd <- case args of
    Symbol "variable" : rest -> runFilterVar rest
  return Null
 where
  runFilterVar :: [Value] -> InterpreterProcess ()
  runFilterVar args = case args of
    [Symbol name, value] -> withGfxCtx (\ctx -> setFilterVar ctx name value)
    _                    -> throwError "Error with functions to filter variable"
