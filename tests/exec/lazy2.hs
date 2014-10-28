f x = 'a'

main = do {
  putChar (f (error "oups"));
  putChar '\n'
  }


