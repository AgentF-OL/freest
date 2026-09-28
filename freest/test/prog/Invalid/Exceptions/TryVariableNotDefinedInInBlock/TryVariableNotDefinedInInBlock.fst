module TryVariableNotDefinedInInBlock where

_ =
  try
    let x = 1 in x; 2
  as res in
    x
  otherwise
    3