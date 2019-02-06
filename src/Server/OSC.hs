module Server.OSC
  ( startOSCServer
  )
where

import           Control.Concurrent             ( ThreadId
                                                , forkIO
                                                )
import           Control.Concurrent.STM         ( atomically
                                                , modifyTVar
                                                )
import           Control.Monad                  ( forever
                                                , mapM_
                                                )
import           Lens.Simple                    ( (^.) )

import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import           Data.List.Split                ( splitOn )

import           Sound.OSC.Datum                ( datum_floating )
import           Sound.OSC.FD                   ( Message
                                                , recvPacket
                                                , udpServer
                                                )
import           Sound.OSC.Packet               ( Bundle(..)
                                                , Message(..)
                                                , Packet(..)
                                                )

import qualified Configuration                 as C
import qualified Configuration.OSC             as CO
import           Improviz                       ( ImprovizEnv )
import qualified Improviz                      as I

import           Language.Ast                   ( Value(Number) )
import           Logging                        ( logDebug
                                                , logError
                                                , logInfo
                                                )

handleMessage :: ImprovizEnv -> Message -> IO ()
handleMessage env (Message addr datem) =
  let path = splitOn "/" addr
  in  case (path, datem) of
        (["", "vars", name], [value]) -> atomically $ modifyTVar
          (env ^. I.externalVars)
          (M.insert name (Number $ fromMaybe 0.0 $ datum_floating value))
        _ -> logError "invalid OSC address"

startOSCServer :: ImprovizEnv -> IO ThreadId
startOSCServer env =
  let config  = env ^. I.config
      oscPort = (config ^. C.osc . CO.port)
  in  do
        logInfo $ "Improviz OSC server listening on port " ++ show oscPort
        socket <- udpServer "127.0.0.1" oscPort
        forkIO $ forever $ do
          packet <- recvPacket socket
          logDebug config $ "Packet received: " ++ show packet
          case packet of
            Packet_Message m -> handleMessage env m
            Packet_Bundle  b -> mapM_ (handleMessage env) (bundleMessages b)
