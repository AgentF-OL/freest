type Render : 1C
type Render = ?type (a : *T). ?a ; ?(a -1-> String) ; !String ; Wait

render : Render -> ()
render c =
  try receiveType c as (@typ, c) in
    try receive c as (x, c) in
      try receive c as (f, c) in
        try sendAndWait (f x) c as _ in ()
        unless print "Failed to close channel"
      unless print "Failed to receive render function"
    unless print "Failed to receive value to render"
  unless print "Failed to receive type of the value to render"

charRenderer : Dual Render -> ()
charRenderer = cancel

_ =
  let (joinEnd, awaitEnd) = channel @ForkJoin in
  forkWith (\c -> render c; join joinEnd) |> charRenderer;
  await 1 awaitEnd