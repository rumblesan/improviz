module Language.ImpVM.StdLib where

import qualified Data.Map                      as M
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( forM_ )
import           Lens.Simple                    ( assign )

import           Language.ImpVM.Types           ( VM
                                                , StackItem(..)
                                                , builtins
                                                )
import           Language.ImpVM.VM              ( pushStack )
import           Gfx.Context                    ( GfxContext )

import           Language.ImpVM.StdLib.Shapes   ( shape )
import           Language.ImpVM.StdLib.Maths    ( mathBuiltIns )
import           Language.ImpVM.StdLib.ColourOps
                                                ( colourBuiltIns )
import           Language.ImpVM.StdLib.MatrixOps
                                                ( gfxMatrix )
import           Language.ImpVM.StdLib.PostEffects
                                                ( postEffectsBuiltIns )
import           Language.ImpVM.StdLib.BlockHandling
                                                ( blockBuiltIns )


builtInFuncs :: M.Map String ([StackItem] -> VM GfxContext ())
builtInFuncs =
  M.fromList
    $  [ ("print" , printValue)
       , ("isNull", isNull)
       , ("shape" , shape)
       , ("matrix", gfxMatrix)
       ]
    ++ mathBuiltIns
    ++ colourBuiltIns
    ++ blockBuiltIns
    ++ postEffectsBuiltIns

addStdLib :: VM GfxContext ()
addStdLib = assign builtins builtInFuncs

printValue :: [StackItem] -> VM es ()
printValue args = forM_ args (liftIO . print)

isNull :: [StackItem] -> VM es ()
isNull args = pushStack $ case args of
  SNull : _ -> SFloat 1
  _         -> SFloat 0
