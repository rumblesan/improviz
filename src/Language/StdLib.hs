module Language.StdLib (
  noop,
  module Language.StdLib.Shapes,
  module Language.StdLib.MatrixOps
) where

import Language.StdLib.Shapes
import Language.StdLib.MatrixOps

import Language.Interpreter.Types (BuiltInFunction)
import Language.LanguageAst ( Value(Null) )

noop :: BuiltInFunction
noop _ = return Null
