module Language.ImpVM.StdLib where

import qualified Data.Map                      as M
import           Control.Monad.IO.Class         ( liftIO )
import           Lens.Simple                    ( assign
                                                , use
                                                )

import           Language.ImpVM.Types
import           Language.ImpVM.VM

import           Gfx.Context

builtInFuncs :: M.Map String (VM GfxContext ())
builtInFuncs = M.fromList [("print", printValue), ("shape", shape)]

addStdLib :: VM GfxContext ()
addStdLib = assign builtins builtInFuncs

printValue :: VM es ()
printValue = do
  v <- popStack
  liftIO $ print v

shape :: VM GfxContext ()
shape = do
  liftIO $ print "Running shape"
  shapeArgs <- popStack
  case shapeArgs of
    SString "cube"      -> cubeS
    SString "sphere"    -> sphereS
    SString "cylinder"  -> cylinderS
    SString "rectangle" -> rectangleS
    SString "line"      -> lineS
    _                   -> setError "Invalid shape command value"
 where
  cubeS = do
    SFloat x <- popStack
    SFloat y <- popStack
    SFloat z <- popStack
    ctx      <- use externalState
    liftIO $ drawCube ctx x y z
  sphereS = do
    SFloat x <- popStack
    SFloat y <- popStack
    SFloat z <- popStack
    ctx      <- use externalState
    liftIO $ drawSphere ctx x y z
  cylinderS = do
    SFloat x <- popStack
    SFloat y <- popStack
    SFloat z <- popStack
    ctx      <- use externalState
    liftIO $ drawCylinder ctx x y z
  rectangleS = do
    SFloat x <- popStack
    SFloat y <- popStack
    ctx      <- use externalState
    liftIO $ drawRectangle ctx x y
  lineS = do
    SFloat x <- popStack
    ctx      <- use externalState
    liftIO $ drawLine ctx x
