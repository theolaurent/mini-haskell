
main =
  let div x y = x + y in
  if div 1 0 == 1 then do { putChar 's'; putChar '\n' } else error "oups"
