
id x = x

fix x = let y = (x :: a => a -> a) in y y
fix2 (x :: a => a -> a) = x x

fixid = fix id
fixidid = fixid id

fix2id = fix2 id

main = do {
   return ()
}
