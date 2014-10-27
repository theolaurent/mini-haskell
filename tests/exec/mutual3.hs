main =
  let {
    even x = if x == 0 then 'e' else odd (x-1);
    odd  x = if x == 0 then 'o' else even (x-1)
    } in
  do {
    putChar (even 0);
    putChar (even 1);
    putChar (even 42);
    putChar '\n'
    }
