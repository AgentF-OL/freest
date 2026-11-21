exception MyCustomException String Int

showExn : MyCustomException -> ()
showExn x@(MyCustomException str n) = print x; print str; print n
showExn x = print x

_ = showExn $ MyCustomException "Hello, Exception!" 1