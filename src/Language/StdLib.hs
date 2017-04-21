module Language.StdLib (noop, module Ops) where

import Language.StdLib.Shapes as Ops
import Language.StdLib.MatrixOps as Ops
import Language.StdLib.ColourOps as Ops
import Language.StdLib.PostEffects as Ops
import Language.StdLib.Maths as Ops
import Language.StdLib.BlockHandling as Ops

import Language.Interpreter.Types (BuiltInFunction)
import Language.Ast ( Value(Null) )

noop :: BuiltInFunction
noop = return Null
