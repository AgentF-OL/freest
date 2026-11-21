exception MyCustomException Int
exception MyWrapperException <- MyCustomException String MyCustomException

showExn : MyCustomException -> ()
showExn x = case x of
  MyWrapperException n1 str wrap@(MyCustomException n2) -> print x; print n1; print str; print wrap; print n2
  _ -> print x

_ = showExn $ MyWrapperException 1 "I am a wrapper!" $ MyCustomException (-1)