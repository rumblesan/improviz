module Language.StdLib.Shapes
  ( addShapesStdLib
  )
where

import           Control.Monad.Except

import           Gfx.Interpreter                ( drawLine
                                                , drawRectangle
                                                , drawCube
                                                , drawCylinder
                                                , drawSphere
                                                )
import           Language.Ast                   ( Value(Symbol, Null, Number) )
import           Language.Interpreter           ( setBuiltIn
                                                , execGfx
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )

addShapesStdLib :: InterpreterProcess ()
addShapesStdLib = setBuiltIn "shape" shape

shape :: [Value] -> InterpreterProcess Value
shape shapeArgs = do
  case shapeArgs of
    Symbol "cube"      : rest -> cubeS rest
    Symbol "sphere"    : rest -> sphereS rest
    Symbol "cylinder"  : rest -> cylinderS rest
    Symbol "rectangle" : rest -> rectangleS rest
    Symbol "line"      : rest -> lineS rest
    _                         -> throwError "Invalid shape command value"
  return Null
 where
  cubeS args = case args of
    [Number x, Number y, Number z] -> execGfx $ drawCube x y z
    _ -> throwError "Wrong number of arguments to shape function"
  sphereS args = case args of
    [Number x, Number y, Number z] -> execGfx $ drawSphere x y z
    _ -> throwError "Wrong number of arguments to shape function"
  cylinderS args = case args of
    [Number x, Number y, Number z] -> execGfx $ drawCylinder x y z
    _ -> throwError "Wrong number of arguments to shape function"
  rectangleS args = case args of
    [Number x, Number y] -> execGfx $ drawRectangle x y
    _ -> throwError "Wrong number of arguments to shape function"
  lineS args = case args of
    [Number x] -> execGfx $ drawLine x
    _          -> throwError "Wrong number of arguments to shape function"
