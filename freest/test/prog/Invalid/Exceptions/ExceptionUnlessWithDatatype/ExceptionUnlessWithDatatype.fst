type Me = ()
data ImAnException = Really | Believe Me

_ =
  try
    raise
  as _ in ()
  unless
    Really -> ()
    Believe me -> me