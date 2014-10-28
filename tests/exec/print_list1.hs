
digits = "0123456789"

nth n l = case l of {
  [] -> error "nth";
  x : s -> if n == 0 then x else nth (n-1) s
  }

print_int n = do {
  if n > 9 then print_int (div n 10) else return ();
  putChar (nth (rem n 10) digits)
  }

print_list l = case l of {
  [] -> putChar '\n';
  x:xs -> do {
    print_int x;
    putChar ',';
    print_list xs
    }
  }

main =
  print_list [1, 2, 3, 42]

