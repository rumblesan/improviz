module Language.StdLib.Shapes
  ( addShapesStdLib
  )
where

import           Control.Monad.Except

import           Gfx.Context                    ( drawLine
                                                , drawCube
                                                , drawRectangle
                                                , drawCylinder
                                                , drawSphere
                                                )
import           Language.Ast                   ( Value(Symbol, Null, Number) )
import           Language.Interpreter           ( setBuiltIn
                                                , useGfxCtx
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
  cubeS :: [Value] -> InterpreterProcess ()
  cubeS args = case args of
    [Number x, Number y, Number z] -> useGfxCtx (\ctx -> drawCube ctx x y z)
    _ -> throwError "Wrong number of arguments to shape function"
  sphereS :: [Value] -> InterpreterProcess ()
  sphereS args = case args of
    [Number x, Number y, Number z] -> useGfxCtx (\ctx -> drawSphere ctx x y z)
    _ -> throwError "Wrong number of arguments to shape function"
  cylinderS :: [Value] -> InterpreterProcess ()
  cylinderS args = case args of
    [Number x, Number y, Number z] ->
      useGfxCtx (\ctx -> drawCylinder ctx x y z)
    _ -> throwError "Wrong number of arguments to shape function"
  rectangleS :: [Value] -> InterpreterProcess ()
  rectangleS args = case args of
    [Number x, Number y] -> useGfxCtx (\ctx -> drawRectangle ctx x y)
    _ -> throwError "Wrong number of arguments to shape function"
  lineS :: [Value] -> InterpreterProcess ()
  lineS args = case args of
    [Number x] -> useGfxCtx (`drawLine` x)
    _          -> throwError "Wrong number of arguments to shape function"
