module SenderCancelled where

sendInt : Int -> !Int ; Wait -> ()
sendInt n c = cancel c

receiveInt : ?Int ; Close -> Maybe Int
receiveInt c =
  try
    receiveAndClose c
  as n in
    Just n
  otherwise
    Nothing

_ = forkWith (sendInt 42) |> receiveInt |> print