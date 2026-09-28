module OtherwiseRaise where

_ =
  try
    raise
  as _ in
    1
  otherwise
    raise