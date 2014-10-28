
-- entiers de Church

to_string n = n (\ x -> 'S' : x) []

print_string s = case s of {
  []    -> do { putChar 'O'; putChar '\n' };
  x : r -> do { putChar x; print_string r } }

print_nat n = print_string (to_string n)

-- les définitions sont ici introduites avec let, pour être polymorphes
-- merci à Martin Clochard (promotion 2010) pour la fonction mpoly
-- permettant de définir ackermann

main =
  let mpoly n =
        let fold l = case l of {
              []  -> (\f -> \s -> s);
              x:n -> (\f -> \s -> f (fold n f s)) } in
        fold (to_string n)
  in
  let zero f s = s in
  let succ n f s = n f (f s) in
  let one = succ zero in
  let two = succ one in
  let three = succ two in
  let four = succ three in
  let ackermann n = n (\f -> \m -> mpoly m f (f (succ zero))) succ in
  print_nat (ackermann three four)

