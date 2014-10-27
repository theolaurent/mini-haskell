main =
  let {
    x = 'a' : y;
    y = 'b' : x;
    nth n l = case l of {
      [] -> error "nth";
      x : s -> if n == 0 then x else nth (n-1) s
      }
   } in
  do { putChar (nth 0 x);
       putChar (nth 1 x);
       putChar (nth 2 x);
       putChar '\n' }
