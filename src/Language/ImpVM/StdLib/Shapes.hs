module Language.ImpVM.StdLib.Shapes
  ( shape
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Lens.Simple                    ( use )

import           Language.ImpVM.Types           ( VM
                                                , StackItem(..)
                                                , externalState
                                                )
import           Language.ImpVM.VM              ( setError )

import           Gfx.Context                    ( GfxContext(..) )

shape :: [StackItem] -> VM GfxContext ()
shape shapeArgs = case shapeArgs of
  SString "cube"      : rest -> cubeS rest
  SString "sphere"    : rest -> sphereS rest
  SString "cylinder"  : rest -> cylinderS rest
  SString "rectangle" : rest -> rectangleS rest
  SString "line"      : rest -> lineS rest
  _                          -> setError "Invalid shape command value"
 where
  cubeS args = case args of
    [SFloat x, SFloat y, SFloat z] -> do
      ctx <- use externalState
      liftIO $ drawCube ctx x y z
    _ -> setError "Invalid cube command"
  sphereS args = case args of
    [SFloat x, SFloat y, SFloat z] -> do
      ctx <- use externalState
      liftIO $ drawSphere ctx x y z
    _ -> setError "Invalid sphere command"
  cylinderS args = case args of
    [SFloat x, SFloat y, SFloat z] -> do
      ctx <- use externalState
      liftIO $ drawCylinder ctx x y z
    _ -> setError "Invalid cylinder command"
  rectangleS args = case args of
    [SFloat x, SFloat y] -> do
      ctx <- use externalState
      liftIO $ drawRectangle ctx x y
    _ -> setError "Invalid rectangle command"
  lineS args = case args of
    [SFloat x] -> do
      ctx <- use externalState
      liftIO $ drawLine ctx x
    _ -> setError "Invalid line command"
