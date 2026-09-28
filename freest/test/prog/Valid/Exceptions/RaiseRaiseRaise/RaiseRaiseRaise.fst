module RaiseRaiseRaise where

_ =
  try
    try
      try
        raise
      as _ in
        ()
      otherwise
        print "Ground"; raise
    as _ in
      ()
    otherwise
      print "1st Floor"; raise
  as _ in
    ()
  otherwise
    print "2nd Floor"