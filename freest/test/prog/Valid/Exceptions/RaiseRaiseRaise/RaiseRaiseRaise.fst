_ =
  try
    try
      try
        raise
      as _ in
        ()
      unless
        print "Ground"; raise
    as _ in
      ()
    unless
      print "1st Floor"; raise
  as _ in
    ()
  unless
    print "2nd Floor"