module UncaughtCancel where

sendInt : Int -> !Int ; Close -> ()
sendInt n c = cancel c

receiveInt : ?Int ; Wait -> Int
receiveInt c = receiveAndWait c

_ = forkWith (sendInt 42) |> receiveInt