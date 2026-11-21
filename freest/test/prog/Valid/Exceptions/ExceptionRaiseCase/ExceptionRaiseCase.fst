_ =
  try
    raise Exception
  as _ in
    ()
  unless
    Exception -> print "Failed successfully!"