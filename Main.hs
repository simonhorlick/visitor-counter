{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Network.WebSockets
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = wsServer

wsServer :: IO ()
wsServer = do
  putStrLn "WebSocket Server Listening on http://0.0.0.0:9160/"
  runServer "0.0.0.0" 9160 handleCounter

-- take a pending connection request, accept it and send back the current counter value
handleCounter :: PendingConnection -> IO ()
handleCounter pending = do
  putStrLn "received connection"
  connection <- acceptRequest pending
  let loop = do
        -- loop forever whenever a message is received from the client send back "1"
        msg <- receiveDataMessage connection
        putStrLn ("received message "++show msg)
        sendTextData connection ("1" :: Text)
        loop
  loop

