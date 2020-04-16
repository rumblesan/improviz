module Language.Interpreter.StdLib
  ( addStdLib
  )
where

import           Language.Interpreter.StdLib.BlockHandling
                                                ( addBlockHandlingStdLib )
import           Language.Interpreter.StdLib.ColourOps
                                                ( addColourStdLib )
import           Language.Interpreter.StdLib.Materials
                                                ( addMaterialStdLib )
import           Language.Interpreter.StdLib.Maths
                                                ( addMathStdLib )
import           Language.Interpreter.StdLib.MatrixOps
                                                ( addMatrixStdLib )
import           Language.Interpreter.StdLib.PostEffects
                                                ( addPostEffectsStdLib )
import           Language.Interpreter.StdLib.Shapes
                                                ( addShapesStdLib )
import           Language.Interpreter.StdLib.Util
                                                ( addUtilStdLib )

import           Language.Interpreter.Types     ( InterpreterProcess )

addStdLib :: InterpreterProcess ()
addStdLib = do
  addMathStdLib
  addMaterialStdLib
  addShapesStdLib
  addMatrixStdLib
  addColourStdLib
  addPostEffectsStdLib
  addBlockHandlingStdLib
  addUtilStdLib
