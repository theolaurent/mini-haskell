horror = let f g h = h (g h) in
  let f1 g h = f (h f g) in
  let f g h = f1 (h f1 g) in
  let f1 g h = f (h f g) in
  let f g h = f1 (h f1 g) in
  let f1 g h = f (h f g) in
  let f g h = f1 (h f1 g) in
  let f1 g h = f (h f g) in
  let f g h = f1 (h f1 g) in
  f

main = horror
  (error "a")
  (error "b")
  (error "c")
  (error "d")
  (error "e")
  (error "f")
  (error "g")
  (error "h")
  (error "i")
  (\x -> do { putChar 'j'; x } )
