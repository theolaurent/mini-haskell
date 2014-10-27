
digits = "0123456789"

nth n l = case l of {
  [] -> error "nth";
  x : s -> if n == 0 then x else nth (n-1) s
  }

print_int n = do {
  if n > 9 then print_int (div n 10) else return ();
  putChar (nth (rem n 10) digits)
  }

main = do {
  print_int 42;
  putChar '\n';
  print_int 99;
  putChar '\n';
  print_int 100;
  putChar '\n'
  }


