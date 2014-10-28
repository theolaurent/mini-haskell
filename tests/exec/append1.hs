
print_list l = case l of {
  [] -> putChar '\n';
  x : r -> do { putChar '*'; print_list r }
}

append l1 l2 = case l1 of {
  [] -> l2;
  x : xs -> x : append xs l2
  }

main = do {
  print_list (append [1,2,3] []);
  print_list (append [] [1,2,3]);
  print_list (append [1,2,3] [4]);
  print_list (append [1] [2,3,4]);
  print_list (append [1,2,3] [4,5,6])
}

