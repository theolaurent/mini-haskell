
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
  x : xs -> do {
    print_int x;
    putChar ',';
    print_list xs
    }
  }

merge l1 l2 = case l1 of {
  [] -> l2;
  x1 : xs1 -> case l2 of {
    [] -> l1;
    x2 : xs2 -> if x1 <= x2 then x1 : merge xs1 l2 else x2 : merge l1 xs2
    }
  }

msort l = case l of {
  [] -> [];
  x : xs -> case xs of {
    [] -> [x];
    y : ys -> split [x] [y] ys
    }
  }

split l1 l2 l = case l of {
  [] -> merge (msort l1) (msort l2);
  x : xs -> split l2 (x : l1) xs
  }

main = do {
  print_list (msort [1, 2, 3]);
  print_list (msort [3, 2, 1]);
  print_list (msort [2, 1, 3]);
  print_list (msort [1, 1, 1, 1, 2]);
  print_list (msort [2, 1, 1, 1, 1]);
  print_list (msort [2, 1, 4, 8, 2, 3, 1, 1, 1])
}


