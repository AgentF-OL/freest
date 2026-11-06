_ =
  try
    try
      try
        "Ground"
      as floor in
        putStrLn floor; raise
      unless
        "Underground"
    as floor in
      floor
    unless
      "1st Floor"
  as floor in
    putStrLn floor
  unless
    putStrLn "Underground"