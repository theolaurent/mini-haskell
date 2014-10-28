
-- entiers de Church

to_string n = n (\ x -> 'S' : x) ['O']

print_string s = case s of {
  []    -> putChar '\n';
  x : r -> do { putChar x; print_string r } }

print_nat n = print_string (to_string n)

-- les définitions sont ici introduites avec let, pour être polymorphes
-- on peut alors définir la puissance

main = let {
  zero f x = x;
  one f x = f x;
  add1 n f x = f (n f x);
  add m n f x = m f (n f x);
  mult m n f = m (n f);
  two = add one one;
  four = mult two two;
  pow m n = n m
  } in
  do {
  print_nat zero;
  print_nat one;
  print_nat (add1 zero);
  print_nat two;
  print_nat four;
  print_nat (pow two four)
  }
