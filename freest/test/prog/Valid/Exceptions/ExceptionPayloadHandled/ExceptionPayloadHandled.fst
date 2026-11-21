exception MyCustomException String

_ =
  try
    raise $ MyCustomException "I was handled with care!"
  as _ in ()
  unless
    MyCustomException str -> print str