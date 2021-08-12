module Language.Interpreter.StdLib.Materials
  ( addMaterialStdLib
  ) where

import           Control.Monad.Except

import           Gfx.Context                    ( setMaterial
                                                , setMaterialVar
                                                )
import           Language.Ast                   ( Value(Null, Symbol) )
import           Language.Interpreter.Types     ( InterpreterProcess
                                                , setBuiltIn
                                                , withGfxCtx
                                                )

addMaterialStdLib :: InterpreterProcess ()
addMaterialStdLib = setBuiltIn "intMaterial" internalMaterial

internalMaterial :: [Value] -> InterpreterProcess Value
internalMaterial materialArgs = do
  cmd <- case materialArgs of
    Symbol "material" : rest -> runMaterial rest
    Symbol "variable" : rest -> runMatVar rest
  return Null
 where
  runMaterial :: [Value] -> InterpreterProcess ()
  runMaterial args = case args of
    [Symbol name] -> withGfxCtx (`setMaterial` name)
    _             -> throwError "Error with functions to material"
  runMatVar :: [Value] -> InterpreterProcess ()
  runMatVar args = case args of
    [Symbol name, value] -> withGfxCtx (\ctx -> setMaterialVar ctx name value)
    _ -> throwError "Error with functions to material variable"
