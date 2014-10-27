
iter f l = case l of {
  [] -> return ();
  x : xs -> do { f x; iter f xs } }

main =
  iter putChar "hello world\n"
