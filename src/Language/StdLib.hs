module Language.StdLib (
  noop,
  module Language.StdLib.Shapes
) where

import Language.StdLib.Shapes

import Language.Interpreter.Types (BuiltInFunction)
import Language.LanguageAst ( Value(Null) )

noop :: BuiltInFunction
noop _ = return Null
