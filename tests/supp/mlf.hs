
id x = x

fix x = let y = (x :: a => a) in y y
fix2 (x :: a => a) = x x
fixid = (fix id)
fixidid = fixid id
--a = (0 :: a => a)
main = do {
   return ()
}
