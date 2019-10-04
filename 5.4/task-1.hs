import Data.Char

data Token = Number Int
           | Plus
           | Minus
           | LeftBrace
           | RightBrace
               deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken x
  | all isDigit x = Just (Number (read x :: Int))
  | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . map (asToken) . words
