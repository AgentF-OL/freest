exception MyCustomException Int
exception MyWrapperException <- ArithmeticException String MyCustomException

showExn : ArithmeticException -> ()
showExn x = case x of
  MyWrapperException str wrap@(MyCustomException n) -> print x; print str; print wrap; print n
  _ -> print x

_ = showExn $ MyWrapperException "I am a wrapper!" $ MyCustomException (-1)