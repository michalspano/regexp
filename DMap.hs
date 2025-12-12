module DMap ( empty
            , insert
            , toList
            , DefaultMap ) where

import Data.Map (Map)
import qualified Data.Map as Map

type DefaultMap k v = (Map k v, v)

empty :: v -> DefaultMap k v
empty d = (Map.empty, d)

insert :: Ord k => k -> (v -> v) -> DefaultMap k v -> DefaultMap k v
insert k f (map, d) = case Map.lookup k map of
    Nothing    -> (Map.insert k (f d) map, d)
    Just found -> (Map.insert k (f found) map, d)

toList :: Ord k => DefaultMap k v -> [(k, v)]
toList = Map.toList . fst