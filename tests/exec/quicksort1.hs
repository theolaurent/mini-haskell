
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

append l1 l2 = case l1 of {
  [] -> l2;
  x : xs -> x : append xs l2
  }

qsort l = case l of {
  [] -> [];
  x : xs -> case xs of {
    [] -> [x];
    y : ys -> partition x [] [] (y : ys)
    }
  }

partition p le gt l = case l of {
  [] -> append (qsort le) (p : qsort gt);
  x : xs -> if x <= p then partition p (x : le) gt xs
            else partition p le (x : gt) xs
  }

main = do {
  print_list (qsort [1, 2, 3]);
  print_list (qsort [3, 2, 1]);
  print_list (qsort [2, 1, 3]);
  print_list (qsort [1, 1, 1, 1, 2]);
  print_list (qsort [2, 1, 1, 1, 1]);
  print_list (qsort [2, 1, 4, 8, 2, 3, 1, 1, 1])
}


