exception MyCustomException

_ =
  try
    try
      try
        "Ground"
      as floor in
        print floor; (raise ArithmeticException : String)
      unless
        "Underground"
    as floor in
      floor
    unless
      ArithmeticException -> "1st Floor"
  as floor in
    print floor
  unless
    ArithmeticException -> print "Underground"