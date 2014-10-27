
-- arithmetique de virgule fixe
-- recision q = 8192 i.e. 13 bits pour la partie decimale

add x y = x+y
sub x y = x-y
mul x y = let t = x * y in div (t + div 8192 2) 8192
divi x y = let t = x * 8192 in div (t + div y 2) y
of_int x = x * 8192

zero = of_int 0
two = of_int 2
four = of_int 4

iter n a b xn yn =
  if n == 10 then True
  else
  let xn2 = mul xn xn in
  let yn2 = mul yn yn in
  if add xn2 yn2 > four then False
  else iter (n+1) a b (add (sub xn2 yn2) a)
                      (add (mul two (mul xn yn)) b)

inside x y =
  iter 0 x y zero zero

run steps =
  let xmin = of_int (-2) in
  let xmax = of_int 1 in
  let deltax = divi (sub xmax xmin) (two * steps) in
  let ymin = of_int (-1) in
  let ymax = of_int 1 in
  let deltay = divi (sub ymax ymin) (of_int steps) in
  let fori i =
        if i >= steps then return () else
          let y = add ymin (mul (of_int i) deltay) in
          let forj j =
                if j >= 2 * steps then return () else
                  let x = add xmin (mul (of_int j) deltax) in
                  do { putChar (if inside x y then '0' else '1');
                       forj (j+1) }
          in
          do { forj 0; putChar '\n'; fori (i+1) }
  in
  fori 0

main =
  run 10
