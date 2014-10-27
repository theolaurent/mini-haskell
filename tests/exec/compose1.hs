main =
  let compose f g = \ x -> f (g x) in
  let id x = x in
  do { compose putChar id 'a'; putChar '\n' }
