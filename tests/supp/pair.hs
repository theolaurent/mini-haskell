
pair x y p = p x y

fst p = p (\ x y -> x)
snd p = p (\ x y -> y)

z1 = pair 'a' 'b'

main = do {
   putChar (fst z1) ;
   putChar '\n' ;
}