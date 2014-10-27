iter f l = case l of {
  [] -> putChar '\n';
  x : r -> do { f x; iter f r }
  }

p x =
  putChar (if x <= 2 then 'a' else 'b')

main =
  iter p [1,2,3,4]





