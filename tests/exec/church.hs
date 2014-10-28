
-- entiers de Church

zero f x = x
one f x = f x
add1 n f x = f (n f x)
add m n f x = m f (n f x)
mult m n f x = m (n f) x

two = add one one
four = mult two two

to_string n = n (\ x -> 'S' : x) ['O']

print_string s = case s of {
  []    -> putChar '\n';
  x : r -> do { putChar x; print_string r } }

print_nat n = print_string (to_string n)

main = do {
  print_nat zero;
  print_nat one;
  print_nat (add1 zero);
  print_nat two;
  print_nat four;
  print_nat (mult four four)
  }
