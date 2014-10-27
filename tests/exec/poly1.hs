
main =
  let id x = x in
  do { putChar 'a';
       putChar (id 'b');
       putChar ((id id) 'c');
       putChar '\n' }