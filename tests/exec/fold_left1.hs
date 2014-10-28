fold_left f acc l = case l of {
  [] -> acc;
  x : r -> fold_left f (f acc x) r
  }

plus x y = x + y

x = fold_left plus 0 [1,2,3,4]

main = do {
  putChar (if x == 10 then '+' else '-');
  putChar (if fold_left plus 42 [] == 42 then '+' else '-');
  putChar '\n'
  }
