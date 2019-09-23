class KnownToGork a where
        stomp :: a -> a

        doesEnrageGork :: a -> Bool

class KnownToMork a where
        stab :: a -> a

        doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
        stompOrStab :: a -> a
        stompOrStab a
          | x && y = (stomp . stab) a
          | x = stomp a
          | y = stab a
          | otherwise = a

          where x = doesEnrageMork a
                y = doesEnrageGork a
