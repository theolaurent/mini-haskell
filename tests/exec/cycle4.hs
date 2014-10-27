
c = '0' : c1
c1 ='1' : c2
c2 = '2' : c

nth n l = case l of {
  [] -> error "nth";
  x : s -> if n == 0 then x else nth (n-1) s
  }

main = do {
  putChar (nth 0 c);
  putChar (nth 7 c);
  putChar (nth 42 c);
  putChar (nth 89 c);
  putChar '\n'
  }

