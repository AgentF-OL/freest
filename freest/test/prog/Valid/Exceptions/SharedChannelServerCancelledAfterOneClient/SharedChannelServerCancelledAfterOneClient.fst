module SharedChannelServerCancelledAfterOneClient where

type GzSession : 1C
type GzSession = !Int ; ?Bool ; Close

type GzService : *C
type GzService = *?GzSession

client : Int -> GzService -> Maybe Bool
client n c =
  try receive_ c as c in
    try c |> send n |> receiveAndClose
    as gz in Just gz
    otherwise print "[Client] Connection to server lost"; Nothing
  otherwise print "[Client] Server unavailable"; Nothing

server : Int -> Dual GzService -> ()
server n s =
  try if n == 0 then raise else accept s as c in
    try let (x, c) = receive c in sendAndWait (x > 0) c
    as _ in server (n - 1) s
    otherwise print "[Server] Connection to client lost"; server (n - 1) s
  otherwise drop s; print "[Server] Shutting down"

_ =
  let (c, s) = channel @GzService in
  fork (\_ -> client 1 c |> print);
  fork (\_ -> client 2 c |> print);
  server 1 s
