exception MyCustomException

_ =
  try
    try
      try
        raise ArithmeticException
      as _ in
        ()
      unless
        MyCustomException -> print "Wrong"
    as _ in
      ()
    unless
      MyCustomException -> print "Wrong"
  as _ in
    ()
  unless
    ArithmeticException -> print "Right"