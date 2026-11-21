exception MyCustomException <- ArithmeticException String

showExn : ArithmeticException -> ()
showExn x@(MyCustomException str) = print x; print str
showExn = print

_ = showExn $ MyCustomException "I am also an ArithmeticException!"