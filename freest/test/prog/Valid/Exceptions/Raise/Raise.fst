module Raise where

_ =
  try
    raise
  as _ in
    ()
  otherwise
    print "Failed successfully!"