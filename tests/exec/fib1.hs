fib n =
  let fib_rec fi_1 fi i =
        if i == n then fi else fib_rec fi (fi_1+fi) (i+1)
  in
  fib_rec 0 1 0

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
  print_int (fib 10); putChar '\n'
  }

