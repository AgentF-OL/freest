child : !Int ; Close -> ()
child c = cancel c; raise

parent : ?Int ; Wait -> ()
parent c = try receiveAndWait c as res in putStrLn res unless putStrLn "Child successfully raised"

_ = forkWith child |> parent