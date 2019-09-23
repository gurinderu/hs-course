instance (Printable a, Printable b) => Printable (a, b) where
        toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

