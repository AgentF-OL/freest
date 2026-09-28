module UncaughtBubbleRaise where

_ =
  try
    try
      try
        try
          try raise
          as _ in ()
          otherwise raise
        as _ in ()
        otherwise raise
      as _ in ()
      otherwise raise
    as _ in ()
    otherwise raise
  as _ in ()
  otherwise raise