exception MyCustomException String
exception MyChildException <- MyCustomException Bool

_ =
  try
    raise $ MyChildException "I was handled with care!" True
  as _ in ()
  unless
    MyChildException str b -> print str; print b
    MyCustomException str -> print str