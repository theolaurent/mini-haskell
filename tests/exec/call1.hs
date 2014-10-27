
iter f n =
  do { f n; f (n+1) }

p n =
  putChar (if n == 41 then 'a' else 'b')

main = do {
  iter p 41;
  putChar '\n'
  }
