exception MyCustomException

_ =
  try
    try
      try
        raise MyCustomException
      as _ in
        ()
      unless
        MyCustomException -> print "Ground"; raise ArithmeticException
    as _ in
      ()
    unless
      ArithmeticException -> print "1st Floor"; raise
  as _ in
    ()
  unless
    Exception -> print "2nd Floor"