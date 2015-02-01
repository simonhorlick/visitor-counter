{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.WebSockets

type ClientId = Int

type Client = (ClientId, Connection)

type ServerState = ([Client], Int)

newServerState :: ServerState
newServerState = ([], 0)

addClient :: Client -> ServerState -> ServerState
addClient client (clients, hits) = (client : clients, hits+1)

removeClient :: Client -> ServerState -> ServerState
removeClient client (clients,hits) =
  (filter ((/= fst client) . fst) clients, hits)

-- Send a message to every client in the clients list
broadcast :: Text -> [Client] -> IO ()
broadcast message clients =
    forM_ clients $ \(_, connection) -> sendTextData connection message

main :: IO ()
main = do
    state <- newMVar newServerState
    putStrLn "Starting server on ws://0.0.0.0:9160"
    runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> ServerApp
application state pending = do
    conn <- acceptRequest pending
    forkPingThread conn 30

    putStrLn "Accepted connection"

    clients <- liftIO $ readMVar state

    let
      clientId   = snd clients
      client     = (clientId, conn)
      -- Remove client and return new state
      disconnect = modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')

    flip finally disconnect $ do
         liftIO $ modifyMVar_ state $ \s -> do
             -- add the client to the connected clients list
             let s' = addClient client s
             -- broadcast the updated number of hits to all connected clients
             broadcast (T.pack (show (snd s'))) (fst s')
             return s'
         -- enter an infinite loop until the client disconnects
         talk conn state client

talk :: Connection -> MVar ServerState -> Client -> IO ()
talk connection state (user, _) = forever $ do
    commandMsg <- receiveDataMessage connection
    -- print out anything the client sends to us (shouldn't ever send anything, but, well...)
    print commandMsg

