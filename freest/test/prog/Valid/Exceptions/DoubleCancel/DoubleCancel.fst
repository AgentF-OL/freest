evilChild : !Int ; Close -> ()
evilChild = cancel

evilParent : ?Int ; Wait -> ()
evilParent = cancel

_ =
  let (joinEnd, awaitEnd) = channel @ForkJoin in
  forkWith (\c -> evilChild c; join joinEnd) |> evilParent;
  await 1 awaitEnd;
  print "Complete"