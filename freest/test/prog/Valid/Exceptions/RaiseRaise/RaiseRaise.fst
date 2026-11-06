_ =
  try
    try
      raise
    as _ in
      ()
    unless
      putStrLn "Inside"; raise
  as _ in
    ()
  unless
    putStrLn "Outside"