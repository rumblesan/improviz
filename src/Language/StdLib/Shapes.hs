module Language.StdLib.Shapes
  ( addShapesStdLib
  )
where

import           Control.Monad.Except

import           Gfx.Ast                        ( GfxCommand(ShapeCommand)
                                                , ShapeGfx(..)
                                                )
import           Language.Ast                   ( Block
                                                , Value(Symbol, Null, Number)
                                                )
import           Language.Interpreter           ( addGfxCommand
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )

addShapesStdLib :: InterpreterProcess ()
addShapesStdLib = setBuiltIn "shape" shape

shape :: [Value] -> Maybe Block -> InterpreterProcess Value
shape shapeArgs _ = do
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
    [Number x, Number y, Number z] -> addGfxCommand $ ShapeCommand $ Cube x y z
    _ -> throwError "Wrong number of arguments to shape function"
  sphereS args = case args of
    [Number x, Number y, Number z] ->
      addGfxCommand $ ShapeCommand $ Sphere x y z
    _ -> throwError "Wrong number of arguments to shape function"
  cylinderS args = case args of
    [Number x, Number y, Number z] ->
      addGfxCommand $ ShapeCommand $ Cylinder x y z
    _ -> throwError "Wrong number of arguments to shape function"
  rectangleS args = case args of
    [Number x, Number y] -> addGfxCommand $ ShapeCommand $ Rectangle x y
    _ -> throwError "Wrong number of arguments to shape function"
  lineS args = case args of
    [Number x] -> addGfxCommand $ ShapeCommand $ Line x
    _          -> throwError "Wrong number of arguments to shape function"
