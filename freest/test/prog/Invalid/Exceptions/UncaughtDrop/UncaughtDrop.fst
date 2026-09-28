module UncaughtDrop where

sendInt : Int -> !Int ; Wait -> ()
sendInt n c = drop c

receiveInt : ?Int ; Close -> Int
receiveInt c = receiveAndClose c

_ = forkWith (sendInt 42) |> receiveInt