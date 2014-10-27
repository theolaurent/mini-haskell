
main =
  let putChar c = error "oups" in
  do { putChar 'a'; putChar 'b'; putChar '\n' }
