import Data.Char (isDigit)
import Data.Foldable

findDigit :: [Char] -> Maybe Char
findDigit = find isDigit

