exception MyCustomException Int
exception NoCreativityException

_ =
  let (f : Int -> Bool) = (> 0) in
  try
    raise $ MyCustomException 42
  as _ in f 0
  unless
    MyCustomException n -> f n
    NoCreativityException -> 42 -- Not a Bool
    _ -> f (-1)