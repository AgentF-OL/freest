module TryVariableDefinedInInnerTry where

_ =
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