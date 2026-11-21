_ =
  try
    try
      raise ArithmeticException
    as _ in
      ()
    unless
      ArithmeticException -> print "Inside"; raise Exception
  as _ in
    ()
  unless
    Exception -> print "Outside"