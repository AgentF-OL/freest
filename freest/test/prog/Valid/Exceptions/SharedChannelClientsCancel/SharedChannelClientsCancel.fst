module SharedChannelClientsCancel where

type GzSession : 1C
type GzSession = !Int ; ?Bool ; Close

type GzService : *C
type GzService = *?GzSession

client : Int -> GzService -> Maybe Bool
client n c =
  try receive_ c as c in cancel c; Nothing
  otherwise print "[Client] Server unavailable"; Nothing

server : Int -> Dual GzService -> ()
server 0 s = cancel s; print "[Server] Shutting down"
server n s =
  let c = accept s in
  try let (x, c) = receive c in sendAndWait (x > 0) c
  as _ in server (n - 1) s
  otherwise print "[Server] Connection to client lost"; server (n - 1) s

_ =
  let (c, s) = channel @GzService in
  fork (\_ -> client 1 c);
  fork (\_ -> client 2 c);
  server 2 s
