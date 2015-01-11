pair x y p = p x y

fst p = p (\ x y -> x)
snd p = p (\ x y -> y)

print_list print_fun l = case l of {
   [] -> return () ;
   hd : tl -> do { print_fun hd ; putChar '-' ; print_list print_fun tl }
}

print_pair print_fun pair = do {
    putChar '(' ;
    print_fun (fst pair) ;
    putChar ',' ; putChar ' ' ;
    print_fun (snd pair) ;
    putChar ')'
}

iter_list f l = case l of {
    [] -> return () ;
    hd : tl -> do { f hd ; putChar '\n' ; iter_list f tl }
}

existential_pair x y (p :: a => (a -> IO ()) -> a -> IO ()) = p x y

cp  = existential_pair putChar 's'
sp  = existential_pair (print_list putChar) "coucou"
pp  = existential_pair (print_pair putChar) (pair '4' '2')
psp = existential_pair (print_pair (print_list putChar)) (pair "Bonjour" "Monde")
lpp = existential_pair (print_list (print_pair putChar)) [ pair 'p' 'p' , pair 'i' 'o' , pair 'n' 'n' , pair 'g' 'g' ]

l = [ cp , sp , pp , psp , lpp ]


apply x y = x y

main = iter_list (\p -> p apply) l
