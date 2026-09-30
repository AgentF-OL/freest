module TryVariableDefinedInInnerTry where

_ = print $
  try
    let x = 1 in x;
    try
      x + 1
    as res in
      x + res
    otherwise
      x
  otherwise
    0