_ =
  try
    raise Exception
  as _ in
    ()
  unless
    print "Failed successfully!"