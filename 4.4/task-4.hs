import Data.Char (isDigit)

findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX = helper . findDigit
  where helper (Just x) = x
        helper (Nothing) = 'X'
