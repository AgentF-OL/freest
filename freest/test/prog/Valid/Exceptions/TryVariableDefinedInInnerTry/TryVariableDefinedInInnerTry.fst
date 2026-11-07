_ = print $
  try
    let x = 1 in x;
    try
      x + 1
    as res in
      x + res
    unless
      x
  as res in res
  unless
    0