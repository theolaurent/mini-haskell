
print_bool b = do {
  if b then putChar 't' else putChar 'f';
  putChar '\n'
  }

main = do {
  print_bool True;
  print_bool False;
  print_bool (1 < 2);
  print_bool (1 == 2)
  }
