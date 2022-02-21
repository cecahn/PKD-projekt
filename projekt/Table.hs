
module Table(Table(), empty, insert, exists, lookup, delete, iterate,
             keys, values, fromList, toList) where

import Prelude hiding (lookup, iterate)
import Data.List (find)

---------------------------------------------------------------------------
--                            INTERFACE                                  --
-- This part of the code is "available" and the things you may depend on --
---------------------------------------------------------------------------
{- empty
  RETURNS an empty table
-}
empty :: Table k v

{- insert t k v
  Insert the mapping (k, v) into t. Overwrites the old value if k is already in t.
  RETURNS a new table, where v has been inserted into t with key k
  EXAMPLES: insert empty                 1 "one" == fromList [(1,"one")]
            insert fromList [(1,"two")]) 1 "one" == fromList [(1,"one")]
-}
insert :: Eq k => Table k v -> k -> v -> Table k v

{- exists t k
  Check if k is present among the keys in t.
  RETURNS true if k exists in t, false otherwise
-}
exists :: Eq k => Table k v -> k -> Bool

{- lookup t k
  Look up the value that is mapped to k in t.
  RETURNS: `Just v` if k is present and has value v, Nothing otherwise
  EXAMPLES: lookup (fromList [(1,"one")]) 1 == Just "one"
            lookup (fromList [(1,"one")]) 2 == Nothing
-}
lookup :: Eq k => Table k v -> k -> Maybe v

{- delete t k
  Remove a key k (and corresponding value) from a table. Does nothing if k is not in t.
  RETURN the table t without a mapping to key k
-}
delete :: Eq k => Table k v -> k -> Table k v

{- iterate t f b0
  Applies the function f in a fold-like manner to all key-value pairs in the table (b0 is
  the initial accumulator value).
  RETURNS: final accumulator value, starting from b0 after applying f over each key-value
          pair in t
  EXAMPLE: 3 == iterate (fromList [('a','b'),('c','d'),('e','f')] ) (\n _ -> n+1) 0
-}
iterate :: Table k v -> (b -> (k, v) -> b) -> b -> b

{- keys t f acc0
  Like `iterate` but only over keys.

  EXAMPLE: "eca" == keys (fromList [('a','b'),('c','d'),('e','f')] ) (\s c -> c:s) ""
-}
keys    :: Table k v -> (b -> k -> b) -> b -> b

{- values t f acc0
  Like `iterate` but only over values.
  EXAMPLE: 228 == values (fromList [('a',97),('A',65),('B',66)] ) (+) 0
-}
values  :: Table k v -> (b -> v -> b) -> b -> b

{- fromList xs
  Given a list of (key,value)-tuples, it inserts all these values.

  If there are multiple values for a given key in the list,
  the LAST value in the list is kept.

  EXAMPLE: fromList [(1,1),(1,2)] == fromList [(1,2)]
-}
fromList :: Eq k => [(k,v)] -> Table k v


{- toList t
  Export a table t as a list of key-value tuples.
  EXAMPLE: (fromList . toList) t == t
-}
toList :: Table k v -> [(k,v)]



---------------------------------------------------------------------------
--                            IMPLEMENTATION                             --
-- Below this line is only for the module authors,                       --
--  Module users should not depend on what it is or how it is written    --
---------------------------------------------------------------------------

{- Table k v
   In T l, the elements of l are pairs (key,val)
     where key is the key and val is the value.
   INVARIANT: there is at most one item with a given key in l.
 -}
newtype Table k v = T [(k, v)] deriving Eq

instance (Show k, Show v) => Show (Table k v) where
  show (T xs) = "fromList " ++ show xs

empty = T []

insert (T t) k v = T $ insert' t k v
  where 
    insert' [] k v = [(k,v)]
    insert' ((k', v'):as) k v
      | k == k'   = (k,v) : as
      | otherwise = (k',v') : insert' as k v

exists (T t) k = any ((==k).fst) t
lookup (T t) k = snd <$> find ((==k).fst) t
delete (T t) k = T $ filter ((/=k).fst) t

iterate (T t) f d = foldl f d t
keys    (T t) f d = foldl f d (map fst t)
values  (T t) f d = foldl f d (map snd t)

toList  (T t) = t
fromList = foldl (\t (k,v) -> insert t k v) empty