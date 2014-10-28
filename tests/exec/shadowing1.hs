
main =
  let error x = x in
  do { putChar 'a'; putChar (error '\n') }

