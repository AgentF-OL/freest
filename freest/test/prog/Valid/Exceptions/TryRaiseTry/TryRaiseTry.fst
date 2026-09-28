module TryRaiseTry where

_ =
  try
    try
      try
        "Ground"
      as floor in
        print floor; raise
      otherwise
        "Underground"
    as floor in
      floor
    otherwise
      "1st Floor"
  as floor in
    print floor
  otherwise
    print "Underground"