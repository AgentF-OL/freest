exception MyCustomException String
exception MyChildException <- MyCustomException Bool

_ =
  try
    raise $ MyChildException "I was handled with care!" True
  as _ in ()
  unless
    ArithmeticException -> ()
    MyCustomException str -> print str
    MyChildException str b -> print str; print b