
len l = case l of {
  [] -> 0;
  x : xs -> 1 + len xs
  }

main = do {
  putChar (if len "hello" == 5 then '+' else '-');
  putChar (if len ['a','b','c'] == 4 then '+' else '-');
  putChar (if len [] == 0 then '+' else '-');
  putChar '\n'
  }

