l = error "oups" : 'a' : error "argh"

main =
  case l of {
    [] -> return ();
    x : xs -> putChar x }
