
id x = x
t a b = b + error a

fix x = let y = (x :: a => a) in y y
fix2 (x :: a => a) = x x
--fix2 (x :: b, a >= (Char -> b) => a) = if True then (\u v -> u + v) else x
--fixid = (fix id)
--fixidid = fixid id
--a = (0 :: a => a)
main = do {
   return ()
}
