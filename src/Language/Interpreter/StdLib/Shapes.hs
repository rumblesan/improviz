module Language.Interpreter.StdLib.Shapes
  ( addShapesStdLib
  )
where

import           Control.Monad.Except

import           Gfx.Context                    ( drawShape )
import           Language.Ast                   ( Value(Symbol, Null, Number) )
import           Language.Interpreter           ( setBuiltIn
                                                , useGfxCtx
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )

addShapesStdLib :: InterpreterProcess ()
addShapesStdLib = setBuiltIn "shape" shape

shape :: [Value] -> InterpreterProcess Value
shape args = case args of
  [Symbol name, Number x, Number y, Number z] ->
    useGfxCtx (\ctx -> drawShape ctx name x y z) >> return Null
  _ -> throwError "Wrong number of arguments to shape function"
