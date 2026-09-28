module RaiseRaise where

_ =
  try
    try
      raise
    as _ in
      ()
    otherwise
      print "Inside"; raise
  as _ in
    ()
  otherwise
    print "Outside"