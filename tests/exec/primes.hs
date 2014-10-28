
from n = n : from (n+1)
nats = from 2

myfilter p l = case l of {
  []     -> [];
  x : xs -> if p x then x : myfilter p xs else myfilter p xs }

sieve l = case l of {
  []     -> [];
  p : xs -> p : sieve (myfilter (\n -> rem n p /= 0) xs) }

primes = sieve nats

digits = "0123456789"

nth n l = case l of {
  [] -> error "nth";
  x : s -> if n == 0 then x else nth (n-1) s
  }

print_int n = do {
  if n > 9 then print_int (div n 10) else return ();
  putChar (nth (rem n 10) digits)
  }

print_first n l = if n == 0 then return () else case l of {
    [] -> error "oups";
    x : xs -> do { print_int x; putChar '\n'; print_first (n-1) xs } }

main =
  print_first 25 primes
