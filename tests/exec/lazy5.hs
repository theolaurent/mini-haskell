nat n = n : nat (n+1)

nth n l = case l of {
  [] -> error "nth";
  x : s -> if n == 0 then x else nth (n-1) s }

print_string l = case l of {
  [] -> putChar '\n';
  x:xs -> do { putChar x; print_string xs } }

main =
  if nth 12 (nat 0) == 12 then print_string "youpi!" else error "oups"

