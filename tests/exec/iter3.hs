
main =
  let iter f l = case l of {
        [] -> return ();
        x : xs -> do { f x; iter f xs } } in
  do { iter error [];
       iter putChar "hello world\n" }
