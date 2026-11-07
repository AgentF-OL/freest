_ =
  try
    try
      raise
    as _ in
      ()
    unless
      print "Inside"; raise
  as _ in
    ()
  unless
    print "Outside"