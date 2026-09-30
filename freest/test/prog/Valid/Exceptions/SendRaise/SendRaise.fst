module SendRaise where

sendSurprise : !Int ; Wait -> ()
sendSurprise c = sendAndWait raise

receiveInt : ?Int ; Close -> Maybe Int
receiveInt c =
  try
    receiveAndClose c
  as n in
    Just n
  otherwise
    Nothing

_ = forkWith sendSurprise |> receiveInt |> print