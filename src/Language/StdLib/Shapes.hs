module Language.StdLib.Shapes
  ( addShapesStdLib
  )
where

import           Control.Monad.Except

import           Gfx.Ast                        ( GfxCommand(ShapeCommand)
                                                , ShapeGfx(..)
                                                )
import           Language.Ast                   ( FuncArg(VarArg)
                                                , Value(Symbol, Null)
                                                )
import           Language.Interpreter           ( addGfxCommand
                                                , getNumberFromNull
                                                , getVariableWithError
                                                , setBuiltIn
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )

addShapesStdLib :: InterpreterProcess ()
addShapesStdLib =
  setBuiltIn "shape" shape [VarArg "name", VarArg "x", VarArg "y", VarArg "z"]

shape :: InterpreterProcess Value
shape = do
  name <- getVariableWithError "name" "Must give shape function name argument"
  case name of
    (Symbol "cube"     ) -> cubeS
    (Symbol "sphere"   ) -> sphereS
    (Symbol "cylinder" ) -> cylinderS
    (Symbol "rectangle") -> rectangleS
    (Symbol "line"     ) -> lineS
    _                    -> throwError "invalid shape command value"
  return Null
 where
  cubeS = do
    vX <- getVariableWithError "x" "Must give cube an X size"
    vY <- getVariableWithError "y" "Must give cube a Y size"
    vZ <- getVariableWithError "z" "Must give cube a Z size"
    let x = getNumberFromNull vX 0
    let y = getNumberFromNull vY 0
    let z = getNumberFromNull vZ 0
    addGfxCommand $ ShapeCommand $ Cube x y z
  sphereS = do
    vX <- getVariableWithError "x" "Must give sphere an X size"
    vY <- getVariableWithError "y" "Must give sphere a Y size"
    vZ <- getVariableWithError "z" "Must give sphere a Z size"
    let x = getNumberFromNull vX 0
    let y = getNumberFromNull vY 0
    let z = getNumberFromNull vZ 0
    addGfxCommand $ ShapeCommand $ Sphere x y z
  cylinderS = do
    vX <- getVariableWithError "x" "Must give cylinder an X size"
    vY <- getVariableWithError "y" "Must give cylinder a Y size"
    vZ <- getVariableWithError "z" "Must give cylinder a Z size"
    let x = getNumberFromNull vX 0
    let y = getNumberFromNull vY 0
    let z = getNumberFromNull vZ 0
    addGfxCommand $ ShapeCommand $ Cylinder x y z
  rectangleS = do
    vX <- getVariableWithError "x" "Must give rectangle an X size"
    vY <- getVariableWithError "y" "Must give rectangle a Y size"
    let x = getNumberFromNull vX 0
    let y = getNumberFromNull vY 0
    addGfxCommand $ ShapeCommand $ Rectangle x y
  lineS = do
    vX <- getVariableWithError "x" "Must give line a size"
    let x = getNumberFromNull vX 0
    addGfxCommand $ ShapeCommand $ Line x
