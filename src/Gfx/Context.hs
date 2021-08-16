module Gfx.Context
  ( GfxContext(..)
  , createGfxContext
  , empty
  ) where

import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , modifyTVar'
                                                , readTVarIO
                                                , writeTVar
                                                )
import           Control.Monad.State.Strict     ( execStateT )

import qualified Gfx.Commands                  as GC
import           Gfx.Engine                     ( GfxEngine
                                                , GraphicsEngine
                                                , resetGfxEngine
                                                )
import           Gfx.PostProcessing             ( AnimationStyle )
import           Language.Ast                   ( Value )

data GfxContext = GfxContext
  { drawShape          :: String -> Float -> Float -> Float -> IO ()
  , rotate             :: Float -> Float -> Float -> IO ()
  , scale              :: Float -> Float -> Float -> IO ()
  , move               :: Float -> Float -> Float -> IO ()
  , colourFill         :: Float -> Float -> Float -> Float -> IO ()
  , noFill             :: IO ()
  , textureFill        :: String -> Float -> IO ()
  , colourStroke       :: Float -> Float -> Float -> Float -> IO ()
  , noStroke           :: IO ()
  , setMaterial        :: String -> IO ()
  , setMaterialVar     :: String -> Value -> IO ()
  , setBackground      :: Float -> Float -> Float -> IO ()
  , pushScope          :: IO ()
  , popScope           :: IO ()
  , setAnimationStyle  :: AnimationStyle -> IO ()
  , setFilterVar       :: String -> Value -> IO ()
  , setDepthChecking   :: Bool -> IO ()
  , reset              :: IO ()
  , renderCode         :: String -> IO ()
  , renderCodeToBuffer :: String -> IO ()
  }

createGfxContext :: TVar GfxEngine -> GfxContext
createGfxContext gfx = GfxContext
  { drawShape          = wrapFourArg gfx GC.drawShape
  , rotate             = wrapThreeArg gfx GC.rotate
  , scale              = wrapThreeArg gfx GC.scale
  , move               = wrapThreeArg gfx GC.move
  , colourFill         = wrapFourArg gfx GC.colourFill
  , noFill             = wrapNoArg gfx GC.noFill
  , textureFill        = wrapTwoArg gfx GC.textureFill
  , colourStroke       = wrapFourArg gfx GC.colourStroke
  , noStroke           = wrapNoArg gfx GC.noStroke
  , setMaterial        = wrapOneArg gfx GC.setMaterial
  , setMaterialVar     = wrapTwoArg gfx GC.setMaterialVariable
  , setBackground      = wrapThreeArg gfx GC.setBackground
  , pushScope          = wrapNoArg gfx GC.pushScope
  , popScope           = wrapNoArg gfx GC.popScope
  , setAnimationStyle  = wrapOneArg gfx GC.setAnimationStyle
  , setFilterVar       = wrapTwoArg gfx GC.setFilterVar
  , setDepthChecking   = wrapOneArg gfx GC.setDepthChecking
  , reset              = resetGfxCtx gfx
  , renderCode         = wrapOneArg gfx GC.renderCode
  , renderCodeToBuffer = wrapOneArg gfx GC.renderCodeToBuffer
  }

empty :: GfxContext
empty = GfxContext { drawShape          = \_ _ _ _ -> print "No GFX Context"
                   , rotate             = \_ _ _ -> print "No GFX Context"
                   , scale              = \_ _ _ -> print "No GFX Context"
                   , move               = \_ _ _ -> print "No GFX Context"
                   , colourFill         = \_ _ _ _ -> print "No GFX Context"
                   , noFill             = print "No Gfx Context"
                   , textureFill        = \_ _ -> print "No Gfx Context"
                   , colourStroke       = \_ _ _ _ -> print "No GFX Context"
                   , noStroke           = print "No Gfx Context"
                   , setMaterial        = \_ -> print "No Gfx Context"
                   , setMaterialVar     = \_ _ -> print "No Gfx Context"
                   , setBackground      = \_ _ _ -> print "No Gfx Context"
                   , pushScope          = print "No Gfx Context"
                   , popScope           = print "No Gfx Context"
                   , setAnimationStyle  = \_ -> print "No Gfx Context"
                   , setFilterVar       = \_ _ -> print "No Gfx Context"
                   , setDepthChecking   = \_ -> print "No Gfx Context"
                   , reset              = print "No Gfx Context"
                   , renderCode         = \_ -> print "No Gfx Context"
                   , renderCodeToBuffer = \_ -> print "No Gfx Context"
                   }

resetGfxCtx :: TVar GfxEngine -> IO ()
resetGfxCtx gfx = atomically $ modifyTVar' gfx resetGfxEngine

wrapNoArg :: TVar GfxEngine -> GraphicsEngine () -> IO ()
wrapNoArg gfx func = do
  ge    <- readTVarIO gfx
  newGe <- execStateT func ge
  atomically $ writeTVar gfx newGe

wrapOneArg :: TVar GfxEngine -> (a -> GraphicsEngine ()) -> a -> IO ()
wrapOneArg gfx func a = do
  ge    <- readTVarIO gfx
  newGe <- execStateT (func a) ge
  atomically $ writeTVar gfx newGe

wrapTwoArg
  :: TVar GfxEngine -> (a -> b -> GraphicsEngine ()) -> a -> b -> IO ()
wrapTwoArg gfx func a b = do
  ge    <- readTVarIO gfx
  newGe <- execStateT (func a b) ge
  atomically $ writeTVar gfx newGe

wrapThreeArg
  :: TVar GfxEngine
  -> (a -> b -> c -> GraphicsEngine ())
  -> a
  -> b
  -> c
  -> IO ()
wrapThreeArg gfx func a b c = do
  ge    <- readTVarIO gfx
  newGe <- execStateT (func a b c) ge
  atomically $ writeTVar gfx newGe

wrapFourArg
  :: TVar GfxEngine
  -> (a -> b -> c -> d -> GraphicsEngine ())
  -> a
  -> b
  -> c
  -> d
  -> IO ()
wrapFourArg gfx func a b c d = do
  ge    <- readTVarIO gfx
  newGe <- execStateT (func a b c d) ge
  atomically $ writeTVar gfx newGe
