exception MyCustomException String (Int -1-> Bool)

_ =
  try
    raise $ MyCustomException "I am a GZ server!" f
  as _ in print $ f (-1)
  unless MyCustomException str func -> print str; print $ func 1
  where
    f : Int -1-> Bool
    f = (> 0)