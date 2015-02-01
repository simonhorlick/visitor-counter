visitor-counter

==

A simple website visitor counter. Each time a page is loaded a websocket
connection is made (this increments the visitor counter) then a stream of
updates is broadcast giving the current cumulative visitor count.

Implementation:

  * Communication should be done over HTML5 WebSockets
  * Server process should be written in haskell
  * Workflow:
    1. A new client connects
    2. The list of currently connected clients and the number of hits is
       (atomically) updated
    3. The number of hits is broadcast to all clients
  * The server should conflate updates if clients cannot keep up with the load
  * Limit the number of concurrent connections?

Looking into TChan usage and ghc STM.

