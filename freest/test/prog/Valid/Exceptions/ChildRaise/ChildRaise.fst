module ChildRaise where

child : !Int ; Close -> ()
child c = raise

parent : ?Int ; Wait -> ()
parent c = try receiveAndWait c as res in print res otherwise print "Child successfully raised"

_ = forkWith child |> parent