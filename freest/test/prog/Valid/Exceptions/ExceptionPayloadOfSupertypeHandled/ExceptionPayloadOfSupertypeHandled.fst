exception MyCustomException String
exception MyChildException <- MyCustomException Bool

_ =
  try
    raise $ MyChildException "I was handled with care!" True
  as _ in ()
  unless
    MyCustomException str -> print str