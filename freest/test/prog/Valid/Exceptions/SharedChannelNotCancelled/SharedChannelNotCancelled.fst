type GzSession : 1C
type GzSession = !Int ; ?Bool ; Close

type GzService : *C
type GzService = *?GzSession

client : Int -> GzService -> Maybe Bool
client n c =
  try receive_ c as c in
    try c |> send n |> receiveAndClose
    as gz in Just gz
    unless putStrLn "[Client] Connection to server lost"; Nothing
  unless putStrLn "[Client] Server unavailable"; Nothing

server : Int -> Dual GzService -> ()
server 0 s = cancel s; putStrLn "[Server] Shutting down"
server n s =
  let c = accept s in
  try let (x, c) = receive c in sendAndWait (x > 0) c
  as _ in server (n - 1) s
  unless putStrLn "[Server] Connection to client lost"; server (n - 1) s

_ =
  let (c, s) = channel @GzService in
  fork (\_ -> client 1 c |> putStrLn);
  fork (\_ -> client 2 c |> putStrLn);
  server 2 s
