type Gift : 1C
type Gift = !Int ; Close

greeks : Gift -> ()
greeks g = sendAndClose raise g

trojans : Dual Gift -> ()
trojans g = try receiveAndWait g as res in print res unless ()

_ = forkWith greeks |> trojans