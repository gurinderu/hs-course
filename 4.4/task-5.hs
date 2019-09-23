maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

listToMaybe :: [a] -> Maybe a
listToMaybe (x : _) = (Just x)
listToMaybe [] = Nothing
