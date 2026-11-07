_ =
  try
    try
      try
        "Ground"
      as floor in
        print floor; raise
      unless
        "Underground"
    as floor in
      floor
    unless
      "1st Floor"
  as floor in
    print floor
  unless
    print "Underground"