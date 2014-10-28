
iter f n =
  if n == 0 then return () else do { f n; iter f (n-1) }

p n =
  putChar (if n <= 2 then 'a' else 'b')

main = do {
  iter p 4;
  putChar '\n'
  }

