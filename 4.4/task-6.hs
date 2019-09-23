import qualified Data.Map as M
import Data.Either
import Data.Maybe
import Control.Monad
import Text.Read
import Data.Char (isSpace)
import Data.List

data Error = ParsingError | IncompleteDataError | IncorrectDataError String 

data Person = Person { firstName :: String, lastName :: String, age :: Int } 

split::Char -> String -> [String]
split ch = uncurry (:) . foldr f ("",[]) where
  f::Char -> (String,[String]) -> (String,[String])
  f cch (tmp,acc) | cch == ch = ("", tmp:acc)
                  | otherwise = (cch:tmp,acc)            
                  
extractData :: M.Map String String -> Either Error Person
extractData m =  lookup m "firstName" >>= \fname->
                 lookup m "lastName" >>= \lname->
                 lookup m "age" >>= readInt >>= \age->Right Person{firstName=fname,lastName=lname,age=age} where
                   lookup :: M.Map String String -> String -> Either Error String
                   lookup m k = mkError (M.lookup k m)

                   mkError (Just x) = Right x
                   mkError _        = Left IncompleteDataError

                   readInt :: String -> Either Error Int
                   readInt x =  mkError2 (readMaybe x :: Maybe Int) x

                   mkError2 (Just x) _ = Right x
                   mkError2 _        v = Left (IncorrectDataError v)

parseKey :: [String] -> Either Error (String,String)
parseKey [x,y] = (Right (trim x,trim y))
parseKey _     = (Left ParsingError)

trim = dropWhileEnd isSpace . dropWhile isSpace


parsePerson :: String -> Either Error Person
parsePerson = sequence . map (parseKey . split '=') . (split '\n') >=> (extractData . M.fromList)


