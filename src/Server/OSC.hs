module Server.OSC
  ( createOSCServer
  ) where

import           Control.Concurrent     (ThreadId, forkIO)
import           Control.Concurrent.STM (TVar, atomically, modifyTVar)
import           Control.Monad          (forever, mapM_)

import qualified Data.Map.Strict        as M
import           Data.Maybe             (catMaybes, fromMaybe)

import           Sound.OSC.Datum        (Datum, datum_floating)
import           Sound.OSC.Datum.Datem  (datum_string)
import           Sound.OSC.FD           (Bundle, Message, recvPacket, udpServer)
import           Sound.OSC.Packet       (Bundle (..), Message (..), Packet (..))

import qualified Configuration          as C
import qualified Configuration.OSC      as CO
import           Improviz               (ImprovizEnv)
import qualified Improviz               as I

import           Language.Ast           (Value (Number))

import           Lens.Simple            ((^.))

handleMessage :: ImprovizEnv -> Message -> IO ()
handleMessage env (Message addr datem) =
  case addr of
    "/vars" ->
      atomically $ modifyTVar (env ^. I.externalVars) (updateVars datem)
    _ -> print "invalid message"

updateVars :: [Datum] -> M.Map String Value -> M.Map String Value
updateVars datem existing =
  let updates = M.fromList $ catMaybes $ combineDatumPairs datem
   in M.union updates existing

combineDatumPairs :: [Datum] -> [Maybe (String, Value)]
combineDatumPairs (name:value:rem) =
  let pair = do
        n <- datum_string name
        v <- datum_floating value
        return (n, Number v)
   in pair : combineDatumPairs rem
combineDatumPairs [] = []
combineDatumPairs _ = []

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
