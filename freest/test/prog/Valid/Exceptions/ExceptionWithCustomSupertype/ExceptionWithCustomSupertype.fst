exception MyCustomSuperException
exception MyCustomException <- MyCustomSuperException

showExn : MyCustomSuperException -> ()
showExn x@MyCustomException = print $ "Hello, " ++ show x ++ "!"
showExn x = print x

_ = showExn MyCustomException