module Utils 
  ( first
  , second
  , withMaybe
  , mapMaybe  -- actually, this is Set.mapMaybe
  , findMaxBy
  , dropExactly
  , splitAtExactly
  , modifyAt
  , lookup2
  , invertSetMap
  , foldlMOver
  )
where

import qualified Data.Set as S
import Data.Set ( Set )
import qualified Data.Map as M
import Data.Map ( Map )
import Data.List ( foldl' )
import Data.Monoid 
import Data.Foldable ( Foldable, foldlM )

first :: (a -> c) -> (a, b) -> (c, b)
first f (x, y) = (f x, y)

-- | Apply function to the second argument of the tuple.
second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)

-- | Like 'maybe' but different argument order.  This allows nicer code layout
--   in some cases.  (This is basically continuation-passing style.)
withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe Nothing r _  = r
withMaybe (Just x) _ k = k x

mapMaybe :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
mapMaybe f = foldl' g S.empty . S.toList
  where 
    g s a = case f a of
              Nothing -> s
              Just b -> S.insert b s

findMaxBy :: Ord a => (a -> Bool) -> Set a -> Maybe a
findMaxBy p s = if S.null ps then Nothing else Just (S.findMax ps)
  where ps = S.filter p s

dropExactly :: Int -> [a] -> Maybe [a]
dropExactly n _ | n `seq` False = undefined
dropExactly n xs | n <= 0 = Just xs
dropExactly n [] = Nothing  -- n > 0
dropExactly n (x:xs) = (x:) `fmap` dropExactly (n-1) xs

-- | Like 'splitAt' but requires the list to have at least @n@ elements.
--   Negative offsets are treated as @0@.
splitAtExactly :: Int -> [a] -> Maybe ([a], [a])
splitAtExactly n _ | n `seq` False = undefined
splitAtExactly n xs | n <= 0 = Just ([],xs)
splitAtExactly n [] = Nothing
splitAtExactly n (x:xs) = first (x:) `fmap` splitAtExactly (n-1) xs

-- | @modify n f e l@ will apply function @f@ at offset @n@ of the input list
--   @l@.  If the input list is shorter than @n@ elements it will be filled by
--   inserting @e@.  For example:
--
--   > modifyAt 2 ('x':) "" ["a","b","c","d"]  ==>  ["a","b","xc","d"]
--   > modifyAt 2 ('x':) "" ["a"]  ==> ["a","","x"]
--   
modifyAt :: Int -> (a -> a) -> a -> [a] -> [a]
modifyAt n f e xs | n < 0 = error "modifyAt called with negative offset"
modifyAt 0 f e []     = [f e]
modifyAt 0 f e (x:xs) = f x : xs
modifyAt n f e []     = e : modifyAt (n-1) f e []
modifyAt n f e (x:xs) = x : modifyAt (n-1) f e xs


lookup2 ::(Ord k1, Ord k2, Monad m) => k1 -> k2 -> Map k1 (Map k2 a) -> m a
lookup2 k1 k2 = M.lookup k2 . M.findWithDefault mempty k1 

invertSetMap :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
invertSetMap m = 
    M.foldWithKey (\a bs m' -> 
         S.fold (\b m'' -> M.insertWith S.union b (S.singleton a) m'') m' bs)
      M.empty m

--t1 = M.fromList [(1,S.fromList "abc"),(2,S.fromList "ac")]

foldlMOver :: (Foldable f, Monad m) => 
             f a -> s -> (a -> s -> m s) -> m s
foldlMOver as s f = foldlM (flip f) s as
