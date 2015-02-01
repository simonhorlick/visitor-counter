-- Thanks to http://www.blaenkdenum.com/posts/live-editing-with-hakyll/#websocket-server
-- which I heavily copied for this.

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map

import Control.Exception (fromException, handle)
import Control.Monad (forever, void)
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (liftIO)

import Data.Text (pack)
import qualified Data.ByteString.Char8 as BC

import Network.WebSockets

-- the websocket server channels
type Path = String
type Channel = TChan String
type NumSubscribers = Integer
type Channels = TVar (Map.Map Path (Channel, NumSubscribers))
type Hits = TVar (Map.Map Path Integer)

-- Lookup the channel for this path (or create one if there are none) then
-- return it.
addSubscriber :: Channels -> Path -> IO Channel
addSubscriber channels path =
  -- needs to be atomic to avoid race conditions
  -- between the read and the update
  liftIO $ atomically $ do
    chans <- readTVar channels

    case Map.lookup path chans of
      Just (ch, refcount) -> do
        modifyTVar' channels $ Map.insert path (ch, refcount + 1)
        dupTChan ch
      Nothing -> do
        ch <- newBroadcastTChan
        modifyTVar' channels $ Map.insert path (ch, 1)
        dupTChan ch

-- Remove a subscriber from this path
removeSubscriber :: Channels -> Path -> IO ()
removeSubscriber channels path =
  -- decrement the ref count of the channel
  -- remove it if no listeners
  atomically $ do
    chans <- readTVar channels
    case Map.lookup path chans of
      Just (ch, refcount) ->
        if (refcount - 1) == 0
          then modifyTVar' channels $ Map.delete path
          else modifyTVar' channels $ Map.insert path (ch, refcount - 1)
      Nothing -> return ()

-- Send a message to all clients on this path (or do nothing if there are no
-- subscribers)
broadcast :: Channels -> String -> String -> IO ()
broadcast channels path body =
  void . forkIO $ atomically $ do
    chans <- readTVar channels

    case Map.lookup path chans of
      Just (ch, _) -> writeTChan ch body
      Nothing -> return ()

-- Called when a client initiates a new WebSocket connection
handleConnection :: Channels -> Hits -> PendingConnection -> IO ()
handleConnection channels hits pending = do

  let request = pendingRequest pending
      path    = tail . BC.unpack $ requestPath request

  conn <- acceptRequest pending

  putStrLn $ "Accepted connection for path /"++path

  -- atomically increment the number of hits
  numhits <- liftIO $ atomically $ do
    hitmap <- readTVar hits
    case Map.lookup path hitmap of
      Just numhits -> do
        modifyTVar' hits $ Map.insert path (numhits + 1)
        return (numhits+1)
      Nothing -> do
        modifyTVar' hits $ Map.insert path 1
        return 1

  -- create a channel for this subscriber
  chan <- addSubscriber channels path

  -- broadcast this new number of page hits to everyone on the channel
  broadcast channels path (show numhits)

  -- sit here sending all updates from the channel until the client disconnects
  handle catchDisconnect . forever . liftIO $
    atomically (readTChan chan) >>= sendTextData conn . pack

  -- reduce the reference count on this channel (not strictly necessary as
  -- we'll never be in a situation where we try to send messages with zero
  -- subscribers)
  removeSubscriber channels path

  where
    catchDisconnect e =
      case fromException e of
        Just ConnectionClosed -> return ()
        _ -> return ()

newChannels :: IO Channels
newChannels = atomically $ newTVar Map.empty

newHits :: IO Hits
newHits = atomically $ newTVar Map.empty

main :: IO ()
main = do
  state <- newChannels
  hits <- newHits
  putStrLn "Starting server on ws://0.0.0.0:9160"
  runServer "0.0.0.0" 9160 $ handleConnection state hits

