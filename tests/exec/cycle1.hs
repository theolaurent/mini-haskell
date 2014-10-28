
c = 0 : 1 : 2 : c

print_list n l =
  if n <= 0 then putChar '\n'
  else case l of {
    [] -> error "print_list";
    x : xs -> do {
      if x == 0 then putChar '.' else putChar '*';
      print_list (n-1) xs
      }
    }

main = do {
  print_list 3 c;
  print_list 7 c
  }

