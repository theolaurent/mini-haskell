
l = [1,2,3]

main = case l of {
  [] -> return ();
  x : l -> do { putChar 'a';
                case l of {
                  [] -> return ();
                  x : l -> do { putChar 'b';
                                case l of {
                                  [] -> return ();
                                  x : l -> do { putChar 'c';
                                                case l of {
                                                  [] -> putChar '\n';
                                                  x : l -> error "oups"
                                                  }
                                              }
                                  }
                              }
                  }
              }
  }
