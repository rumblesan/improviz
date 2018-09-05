module Server.OSC
  ( createOSCServer
  ) where

import           Control.Concurrent     (ThreadId, forkIO)
import           Control.Concurrent.STM (TVar, atomically, modifyTVar)
import           Control.Monad          (forever, mapM_)

import           Data.List.Split        (splitOn)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)

import           Sound.OSC.Datum        (datum_floating)
import           Sound.OSC.FD           (Bundle, Message, recvPacket, udpServer)
import           Sound.OSC.Packet       (Bundle (..), Message (..), Packet (..))

import qualified Configuration          as C
import qualified Configuration.OSC      as CO
import           Improviz               (ImprovizEnv)
import qualified Improviz               as I

import           Language.Ast           (Value (Number))

import           Lens.Simple            ((^.))

handleMessage :: ImprovizEnv -> Message -> IO ()
handleMessage env (Message addr datums) =
  let path = splitOn "/" addr
   in case (path, datums) of
        (["", "vars", name], [value]) ->
          atomically $
          modifyTVar
            (env ^. I.externalVars)
            (M.insert name (Number $ fromMaybe 0.0 $ datum_floating value))
        _ -> print "invalid message"

createOSCServer :: ImprovizEnv -> IO ThreadId
createOSCServer env =
  let oscPort = (env ^. I.config . C.osc . CO.port)
   in do print $ "OSC server on port " ++ show oscPort
         socket <- udpServer "127.0.0.1" oscPort
         forkIO $ forever $ do
           packet <- recvPacket socket
           print $ "Packet received: " ++ show packet
           case packet of
             Packet_Message m -> handleMessage env m
             Packet_Bundle b  -> mapM_ (handleMessage env) (bundleMessages b)
