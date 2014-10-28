
rev_aux acc l = case l of {
  [] -> acc;
  x : r -> rev_aux (x : acc) r
  }

rev l = rev_aux [] l

-- pairs

pair x y = [x, y]
first x = case x of { [] -> error "first"; x : xs -> x }
second x = case x of { [] -> error "second"; x : xs -> first xs }

-- queues

empty = pair [] []

push x q = pair (x : first q) (second q)

top q = case second q of {
  [] -> case rev (first q) of {
     [] -> error "top";
     x : o -> x };
  x : o -> x }

pop q = case second q of {
  [] -> case rev (first q) of {
     [] -> error "pop";
     x : o -> pair [] o };
  x : o -> pair (first q) o }

print_q q =
  let print_l l = case l of {
    [] -> putChar ';';
    x : r -> do { putChar '*'; print_l r } } in
  do { print_l (first q); print_l (rev (second q)); putChar '\n' }

q = push 1 (push 2 (push 3 empty))

main = do {
  print_q q;
  putChar (if top q == 3 then '+' else '-'); putChar '\n';
  let r = pop q in
  do {
    print_q r;
    putChar (if top r == 2 then '+' else '-'); putChar '\n';
    let s = pop r in
    do {
      print_q s;
      putChar (if top s == 1 then '+' else '-'); putChar '\n'
      }
    }
  }
