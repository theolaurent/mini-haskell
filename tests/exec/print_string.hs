
print_string l = case l of {
  [] -> putChar '\n';
  x:xs -> do {
    putChar x;
    print_string xs
    }
  }

main =
  print_string "hello world!"

