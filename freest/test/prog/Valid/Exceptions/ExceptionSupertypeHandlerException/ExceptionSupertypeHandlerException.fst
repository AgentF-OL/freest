exception MyCustomException

_ =
  try
    raise MyCustomException
  as _ in ()
  unless Exception -> print "Failed successfully!"