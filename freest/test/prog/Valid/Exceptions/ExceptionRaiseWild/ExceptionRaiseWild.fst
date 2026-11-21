_ =
  try
    raise Exception
  as _ in
    ()
  unless
    _ -> print "Failed successfully!"