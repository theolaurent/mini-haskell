
division x y =
  if y == 0 then error "division by zero" else div x y

main =
  if division 4 0 == 0 then return () else return ()
