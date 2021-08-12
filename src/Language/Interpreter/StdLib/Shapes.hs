module Language.Interpreter.StdLib.Shapes
  ( addShapesStdLib
  ) where

import           Control.Monad.Except

import           Gfx.Context                    ( drawShape )
import           Language.Ast                   ( Value(Null, Number, Symbol) )
import           Language.Interpreter.Types     ( InterpreterProcess
                                                , setBuiltIn
                                                , withGfxCtx
                                                )

addShapesStdLib :: InterpreterProcess ()
addShapesStdLib = setBuiltIn "shape" shape

shape :: [Value] -> InterpreterProcess Value
shape args = case args of
  [Symbol name, Number x, Number y, Number z] ->
    withGfxCtx (\ctx -> drawShape ctx name x y z) >> return Null
  _ -> throwError "Wrong number of arguments to shape function"
