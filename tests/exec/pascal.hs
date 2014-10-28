
print_list n l =
  if n <= 0 then putChar '\n'
  else case l of {
    [] -> error "print_list";
    x : xs -> do {
      if x == 0 then putChar '.' else putChar '*';
      print_list (n-1) xs
      }
    }

make n v =
  if n == 0 then [] else v : make (n-1) v

update_row prev r = case r of {
  [] -> [];
  x : xs -> (rem (x + prev) 7) : update_row x xs
  }

compute_row r = case r of {
  [] -> error "compute_row";
  x : xs -> 1 : update_row 1 xs
  }

loop n i r =
  if i < n then
    let s = compute_row r in
    do {
      print_list i s;
      loop n (i+1) s
      }
  else
    return ()

main =
  let n = 42 in
  loop n 0 (make (n+1) 0)

