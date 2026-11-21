exception MyCustomException
exception MyChildException <- MyCustomException
exception MyUnrelatedException

_ =
  try
    try
      try
        raise MyCustomException
      as _ in ()
      unless ArithmeticException -> ()
    as _ in ()
    unless MyUnrelatedException -> ()
  as _ in ()
  unless MyChildException -> ()
      