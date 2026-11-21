exception MyCustomException Int
exception NoCreativityException

_ =
  let (f : Int -> Bool) = (> 0) in
  try
    raise $ MyCustomException 42
  as _ in f -- Not a Bool
  unless
    MyCustomException n -> f n
    NoCreativityException -> True
    _ -> f (-1)