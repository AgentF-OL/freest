exception MyCustomException Int
exception MyWrapperException <- MyCustomException MyCustomException

showExn : MyCustomException -> ()
showExn x = case x of
  MyWrapperException n wrap -> print (show x ++ " " ++ show n); showExn wrap
  MyCustomException n -> print (show x ++ " " ++ show n)
  _ -> print "Who am I?"

_ =
  showExn $ MyWrapperException 42 $ MyWrapperException 42 $ MyWrapperException 42 $ MyCustomException 42;
  print "I am a matrioska"