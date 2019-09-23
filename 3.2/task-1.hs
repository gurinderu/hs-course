import Data.Char

readDigits :: String -> (String, String)
readDigits x = span isDigit x
