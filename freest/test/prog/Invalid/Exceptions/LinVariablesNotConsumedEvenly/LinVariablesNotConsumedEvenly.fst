type Render : 1C
type Render = ?type (a : *T). ?(a -1-> String) ; ?a ; !String ; Wait

render : Render -> ()
render c =
  try receiveType c as (@typ, c) in
    try receive c as (f, c) in
      try receive c as (x, c) in
        try sendAndWait (f x) c as _ in ()
        unless print "Failed to close channel"
      unless print "Failed to receive value to render" -- f consumed in the 'in' block but not here
    unless print "Failed to receive render function"
  unless print "Failed to receive type of the value to render"

charRenderer : Dual Render -> ()
charRenderer c =
  let c = c |> sendType @Char |> send 'F' |> send showChar in
  try receiveAndClose c as str in print str
  unless print "Failed to render string"
  where
    showChar : Char -1-> String
    showChar c = "My favourite char is " ++ show c

_ =
  let (joinEnd, awaitEnd) = channel @ForkJoin in
  forkWith (\c -> render c; join joinEnd) |> charRenderer;
  await 1 awaitEnd