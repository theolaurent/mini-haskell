
get a i = case a of {
  [] -> error "get";
  x : s -> if i == 0 then x else get s (i-1)
  }

set a i v = case a of {
  [] -> [];
  x : s -> if i == 0 then v : s else x : set s (i-1) v
  }

print_bool b = do {
  if b then putChar 't' else putChar 'f';
  putChar '\n'
  }

main =
  let s = [1, 2, 3] in
  do {
    print_bool (get s 0 == 1);
    print_bool (get s 1 == 2);
    let t = set s 1 4 in
    print_bool (get t 1 == 4)
    }

