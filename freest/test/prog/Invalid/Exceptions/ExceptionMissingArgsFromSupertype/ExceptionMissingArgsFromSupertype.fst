exception MyCustomException String
exception MyChildException <- MyCustomException Int

_ =
  try
    raise $ MyChildException "Do not forget the String of my parent!" 0
  as _ in 42
  unless MyChildException n -> n -- MyChildException String Int