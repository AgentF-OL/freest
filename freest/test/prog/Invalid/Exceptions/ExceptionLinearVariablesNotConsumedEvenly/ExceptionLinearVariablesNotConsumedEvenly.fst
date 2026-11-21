exception MyCustomException Int
exception NoCreativityException

_ =
  let (f : Int -1-> Bool) = (> 0) in
  try
    raise $ MyCustomException 42
  as _ in f 0
  unless
    MyCustomException n -> f n
    NoCreativityException -> True -- linear variable f not consumed
    _ -> f (-1)