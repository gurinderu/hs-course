import qualified Data.List as L
import Prelude hiding (lookup)

class MapLike m where
        empty :: m k v

        lookup :: Ord k => k -> m k v -> Maybe v

        insert :: Ord k => k -> v -> m k v -> m k v

        delete :: Ord k => k -> m k v -> m k v

        fromList :: Ord k => [(k, v)] -> m k v
        fromList [] = empty
        fromList ((k, v) : xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap{getListMap :: [(k, v)]}
                        deriving (Eq, Show)

instance MapLike ListMap where
        empty = ListMap []
        lookup k (ListMap xs) = fmap snd (L.find (((==) k) . fst) xs)
        insert k v m = insert' k v (delete k m)
          where insert' k v (ListMap xs) = (ListMap $ (k, v) : xs)
        delete k (ListMap xs) = ListMap $ L.filter (not . ((==) k) . fst) xs

