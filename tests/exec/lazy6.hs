l = error "oups" : 'a' : error "argh"

print_string l = case l of {
  [] -> putChar '\n';
  x:xs -> do { putChar x; print_string xs } }

main =
  case l of {
    [] -> return ();
    x : xs -> print_string "ouf!" }
