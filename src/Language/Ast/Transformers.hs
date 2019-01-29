module Language.Ast.Transformers where

import           Data.Set                            (Set (..))

import           Language.Ast                        (Program)
import           Language.Ast.Transformers.Globalise (globalise)

transform :: Set String -> Program -> Program
transform = globalise
