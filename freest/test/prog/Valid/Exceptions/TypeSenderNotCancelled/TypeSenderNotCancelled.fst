module TypeSenderNotCancelled where

type Render : 1C
type Render = ?type a. ?(a -1-> String) ; ?a ; !String ; Wait

render : Render -> ()
render c =
  try receiveType c as (@typ, c) in
    try receive c as (f, c) in
      try receive c as (x, c) in
        try sendAndWait (f x) c as _ in ()
        otherwise print "Failed to close channel"
      otherwise print "Failed to receive value to render"
    otherwise print "Failed to receive render function"
  otherwise print "Failed to receive type of the value to render"

charRenderer : Dual Render -> String
charRenderer c =
  let c = c |> sendType @Char |> send showChar |> send 'F' in
  try receiveAndClose c as str in str
  otherwise "Failed to render string"
  where
    showChar : Char -1-> String
    showChar c = "My favourite char is " ++ show c

_ = forkWith render |> charRenderer |> print