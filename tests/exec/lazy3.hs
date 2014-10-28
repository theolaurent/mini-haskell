main =
  let x = error "oups" in
  do { putChar 'y'; putChar '\n' }
