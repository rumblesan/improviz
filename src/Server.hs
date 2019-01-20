module Server
  ( serveComs
  ) where

import           Control.Monad     (void, when)
import           Lens.Simple       ((^.))

import           Configuration     (ImprovizConfig)
import qualified Configuration     as C
import qualified Configuration.OSC as CO
import           Improviz          (ImprovizEnv)
import qualified Improviz          as I

import           Server.Http
import           Server.OSC

serveComs :: ImprovizEnv -> IO ()
serveComs env = do
  when (env ^. I.config . C.osc . CO.enabled) $ void $ startOSCServer env
  void $ startHttpServer env
