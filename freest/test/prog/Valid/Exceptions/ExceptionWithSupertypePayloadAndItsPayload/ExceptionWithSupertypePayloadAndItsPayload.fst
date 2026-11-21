exception MyCustomException String
exception MyChildException <- MyCustomException Int

showExn : MyCustomException -> ()
showExn x@(MyChildException str n) = print x; print str; print n
showExn = print

_ = showExn $ MyChildException "I am also a MyCustomException with its payload!" 42