data Log a = Log [String] a

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog l f
  = let Log msgA a = l
        Log msgB b = f a
      in Log (msgA ++ msgB) b

