_ = putStrLn $
  try
    let x = 1 in x;
    try
      x + 1
    as res in
      x + res
    unless
      x
  unless
    0