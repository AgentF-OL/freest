module TrojanRaise where

type Gift : 1C
type Gift = !Int ; Close

greeks : Gift -> ()
greeks g = sendAndClose raise g

trojans : Dual Gift -> Int
trojans g = receiveAndWait g

_ = forkWith greeks |> trojans