exception MyCustomException
exception MyHandledException <- MyCustomException

_ =
  try
    raise MyHandledException
  as _ in ()
  unless MyCustomException -> print "Failed successfully!"