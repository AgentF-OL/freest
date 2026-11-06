_ =
  try
    try
      try
        raise
      as _ in
        ()
      unless
        putStrLn "Ground"; raise
    as _ in
      ()
    unless
      putStrLn "1st Floor"; raise
  as _ in
    ()
  unless
    putStrLn "2nd Floor"