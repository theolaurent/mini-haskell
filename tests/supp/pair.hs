
pair x y p = p x y

fst p = p (\ x y -> x)
snd p = p (\ x y -> y)

z = pair 'a' 'b'

print_pair print_fun pair = do {
    putChar '(' ;
    print_fun (fst pair) ;
    putChar ',' ; putChar ' ' ;
    print_fun (snd pair) ;
    putChar ')'
}

print_list print_fun l = case l of {
   [] -> return () ;
   hd : tl -> do { print_fun hd ; putChar '-' ; print_list print_fun tl }
}

zip l = case fst l of {
   [] -> [] ;
   hd1 : tl1 -> case snd l of {
       [] -> [] ;
       hd2 : tl2 ->  (pair hd1 hd2) : zip (pair tl1 tl2)
   }
}

unzip l = case l of {
   [] -> pair [] [] ;
   hd : tl ->
      let tlbis = unzip tl in
      pair (fst hd : fst tlbis) (snd hd : snd tlbis)
}


pl = pair "Bonjour" "Monde"
lp = zip pl
plbis = unzip lp
lpbis = zip plbis

main = do {
   print_pair putChar z ;
   putChar '\n' ;
   print_pair (print_list putChar) pl ;
   putChar '\n' ;
   print_list (print_pair putChar) lp ;
   putChar '\n' ;
   print_pair (print_list putChar) plbis ;
   putChar '\n' ;
   print_list (print_pair putChar) lpbis ;
}
