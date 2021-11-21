module TestHelpers.GfxContext
  ( createGfxContextHelpers
  , getOutputGfx
  ) where

import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , modifyTVar'
                                                , newTVarIO
                                                , readTVarIO
                                                )

import           Gfx.Context                    ( GfxContext(..) )
import           TestHelpers.GfxAst

createGfxContextHelpers :: IO (TVar [GfxCommand], GfxContext)
createGfxContextHelpers = do
  gfx <- newTVarIO []
  return (gfx, createTestGfxContext gfx)

getOutputGfx :: TVar [GfxCommand] -> IO [GfxCommand]
getOutputGfx gfx = reverse <$> readTVarIO gfx

createTestGfxContext :: TVar [GfxCommand] -> GfxContext
createTestGfxContext gfx = GfxContext
  { drawShape = \name x y z -> addAst gfx (ShapeCommand $ ShapeGfx name x y z)
  , rotate             = \x y z -> addAst gfx (MatrixCommand $ Rotate x y z)
  , scale              = \x y z -> addAst gfx (MatrixCommand $ Scale x y z)
  , move               = \x y z -> addAst gfx (MatrixCommand $ Move x y z)
  , colourFill         = \r g b a ->
                           addAst gfx (ColourCommand $ Fill $ ColourStyle r g b a)
  , noFill             = addAst gfx (ColourCommand NoFill)
  , textureFill        = \name frame -> addAst
                           gfx
                           (ColourCommand $ Fill $ TextureStyle name frame)
  , colourStroke       = \r g b a -> addAst gfx (ColourCommand $ Stroke r g b a)
  , noStroke           = addAst gfx (ColourCommand NoStroke)
  , setMaterial        = \_ -> print "No set material command"
  , setMaterialVar     = \_ _ -> print "No set material var command"
  , setBackground      = \_ _ _ -> print "No background command"
  , pushScope          = addAst gfx (ScopeCommand PushScope)
  , popScope           = addAst gfx (ScopeCommand PopScope)
  , setAnimationStyle  = \_ -> print "No animation style command"
  , setFilterVar       = \_ _ -> print "No set filter var command"
  , setDepthChecking   = \_ -> print "No depth checking command"
  , setBlendFunction   = \_ -> print "No blend function command"
  , reset              = print "No reset command"
  , renderCode         = \_ -> print "No renderCode command"
  , renderCodeToBuffer = \_ -> print "No renderCodeToBuffer command"
  }

addAst :: TVar [GfxCommand] -> GfxCommand -> IO ()
addAst gfx cmd = atomically $ modifyTVar' gfx (cmd :)
