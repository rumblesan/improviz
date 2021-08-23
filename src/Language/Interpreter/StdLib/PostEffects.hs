module Language.Interpreter.StdLib.PostEffects
  ( addPostEffectsStdLib
  ) where

import           Control.Monad.Except
import           Data.Maybe                     ( maybe )

import           Gfx.PostProcessing             ( AnimationStyle(..) )

import           Gfx.Context                    ( setAnimationStyle
                                                , setBlendFunction
                                                , setFilterVar
                                                )
import           Gfx.OpenGL                     ( valueToBlendFactor )
import           Language.Ast                   ( Value(Null, Symbol, VList) )
import           Language.Interpreter.Types     ( InterpreterProcess
                                                , setBuiltIn
                                                , withGfxCtx
                                                )

addPostEffectsStdLib :: InterpreterProcess ()
addPostEffectsStdLib = do
  setBuiltIn "animationStyle" animationStyle
  setBuiltIn "postProcess"    internalPostProcess
  setBuiltIn "blendFunc"      blendFunc

animationStyle :: [Value] -> InterpreterProcess Value
animationStyle args = do
  case args of
    [Symbol name] -> withGfxCtx (`setAnimationStyle` (UserFilter name))
    _             -> withGfxCtx (`setAnimationStyle` NormalStyle)
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

blendFunc :: [Value] -> InterpreterProcess Value
blendFunc [VList [first, second], VList [third, fourth]] = do
  a <- maybe (throwError "invalid blend factor value")
             return
             (valueToBlendFactor first)
  b <- maybe (throwError "invalid blend factor value")
             return
             (valueToBlendFactor second)
  c <- maybe (throwError "invalid blend factor value")
             return
             (valueToBlendFactor third)
  d <- maybe (throwError "invalid blend factor value")
             return
             (valueToBlendFactor fourth)
  withGfxCtx (\ctx -> setBlendFunction ctx ((a, b), (c, d)))
  return Null
blendFunc _ = throwError "Error with arguments to blend func"
