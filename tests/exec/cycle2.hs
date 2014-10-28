
c = '0' : '1' : '2' : c

hd l = case l of { [] -> error "hd"; x : xs -> x }
tl l = case l of { [] -> error "hd"; x : xs -> xs }

main = do {
  putChar (hd c);
  putChar (hd (tl c));
  putChar '\n'
  }

