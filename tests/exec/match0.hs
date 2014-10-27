
g l = case l of {
  [] -> 'n';
  x : xs -> 'c'
  }

nil = []
l1 = 1 : nil
l2 = 2 : l1

main = do {
  putChar (g []);
  putChar (g [1]);
  putChar (g [1,2]);
  putChar (g nil);
  putChar (g l1);
  putChar (g l2);
  putChar '\n'
  }