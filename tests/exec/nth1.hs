
digits = "0123456789"

nth n l = case l of {
  [] -> error "nth";
  x : s -> if n == 0 then x else nth (n-1) s
  }

main = do {
  putChar (nth 4 digits);
  putChar (nth 2 digits);
  putChar '\n';
  putChar (nth 8 digits);
  putChar (nth 9 digits);
  putChar '\n'
}
