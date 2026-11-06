_ =
  try
    let x = 1 in x; raise
  as res in
    res
  unless
    x