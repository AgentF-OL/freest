type GzSession : 1C
type GzSession = ?Int ; !Bool ; Wait

type GzService, Answer : *C
type GzService = *!GzSession
type Answer = *!String

client : Int -> GzService -> Answer -> ()
client n c answer =
  try request c as c in
    try c |> send n |> receiveAndClose
    as gz in send_ (show gz) answer; ()
    unless send_ "[Client] Connection to server lost" answer; ()
  unless send_ "[Client] Server unavailable" answer; ()

server : Int -> Dual GzService -> Answer -> ()
server 0 s answer = cancel s; send_ "[Server] Shutting down" answer; ()
server n s answer = 
  let c = accept s in
  try
    let (x, c) = receive c in
      if n == 2
      then cancel c
      else sendAndWait (x > 0) c
  as _ in server (n - 1) s answer
  unless send_ "[Server] Connection to client lost" answer; server (n - 1) s answer

test : Dual Answer -> ()
test c =
  if check (receive_ c) (receive_ c) (receive_ c)
  then putStrLn "nondeterministic test passed"
  else putStrLn "nondeterministic test failed"
  where
    -- | String equality (in the Prelude, (==) is on Int only)
    eq : String -> String -> Bool
    eq [] [] = True
    eq [] _ = False
    eq _ [] = False
    eq (x::xs) (y::ys) = ord x == ord y && eq xs ys

    valid : String -> Bool
    valid xs = eq xs "[Client] Connection to server lost"
      || eq xs "True"
      || eq xs "[Server] Shutting down"
    
    check : String -> String -> String -> Bool
    check xs ys zs =
      valid xs && valid ys && valid zs &&
      not (eq xs ys) && not (eq ys zs) && not (eq xs zs)

_ =
  let (c, s) = channel @GzService in
  let (answer, checker) = channel @Answer in
  fork (\_ -> client 1 c answer);
  fork (\_ -> client 2 c answer);
  server 2 s answer;
  test checker
