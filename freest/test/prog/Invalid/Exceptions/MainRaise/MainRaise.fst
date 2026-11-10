parent : !Int ; Close -> ()
parent c = cancel c; raise

child : ?Int ; Wait -> ()
child c = try receiveAndWait c as res in print res unless ()

_ = forkWith child |> parent