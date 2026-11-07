type Render : 1C
type Render = ?type a. ?(a -1-> String) ; ?a ; !String ; Wait

render : Render -> ()
render c =
  try receiveType c as (@typ, c) in
    try receive c as (f, c) in
      try receive c as (x, c) in
        try sendAndWait (f x) c as _ in ()
        unless print "Failed to close channel"
      unless print "Failed to receive value to render"
    unless print "Failed to receive render function"
  unless print "Failed to receive type of the value to render"

charRenderer : Dual Render -> String
charRenderer c = cancel c; ";)"

_ = forkWith render |> charRenderer |> print