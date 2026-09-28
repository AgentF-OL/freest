module TryVariableNotDefinedInOtherwiseBlock where

_ =
  try
    let x = 1 in x; raise
  as res in
    res
  otherwise
    x