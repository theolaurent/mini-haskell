main =
  let f x = 'r' in
  do { putChar (f (error "oups")); putChar '\n' }


