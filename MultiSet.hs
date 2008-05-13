module MultiSet 
  ( MultiSet
  , empty
  , singleton
  , insert
  , fromList
  , delete
  , union
  , isSubsetOf
  , remSubset'
  , members
  , (\\)
  , toList
  )
where

import PPrint 

import Text.PrettyPrint.HughesPJ as PP hiding ( empty )
import qualified Data.Map as M 
import Data.List ( foldl' )
import Control.Monad ( liftM )

------------------------------------------------------------------------------

newtype MultiSet a = MS (M.Map a Int)

instance PPrint a => PPrint (MultiSet a) where
    pshow (MS m) = braces $ fsep $ punctuate comma
                   $ map (\(a,n) -> pshow a <+> char 'x' <+> int n)
                   $ M.toList m

empty :: MultiSet a
empty = MS $ M.empty

singleton :: a -> MultiSet a
singleton x = MS $ M.singleton x 1

insert :: Ord a => a -> MultiSet a -> MultiSet a
insert a (MS ms) = MS $ M.alter inc a ms
  where inc Nothing = Just 1
        inc (Just x) = Just $! 1 + x

fromList :: Ord a => [a] -> MultiSet a
fromList = foldl' (flip insert) empty
--  where ins x m = insert x m

delete :: Ord a => a -> MultiSet a -> MultiSet a
delete a (MS ms) = MS $ M.alter dec a ms
  where dec Nothing = Nothing
        dec (Just x) | x == 1 = Nothing
                     | otherwise = Just $! x - 1

union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union (MS m1) (MS m2) = MS $ M.unionWith (+) m1 m2

isSubsetOf :: Ord a => MultiSet a -> MultiSet a -> Bool
isSubsetOf (MS m1) (MS m2) = M.foldWithKey checkMem True m1
  where checkMem _ _ False = False
        checkMem x xn r = r && xn <= M.findWithDefault 0 x m2

remSubset' :: Ord a =>  MultiSet a -> MultiSet a -> Maybe (MultiSet a)
remSubset' (MS m1) (MS m2) = liftM MS $ M.foldWithKey remM1 (Just m2) m1
  where remM1 _ _ Nothing = Nothing
        remM1 x xn (Just m) = 
            let xn' = M.findWithDefault 0 x m - xn in
            if xn' < 0 then Nothing
              else if xn' == 0 then Just $ M.delete x m 
                              else Just $ M.insert x xn' m 

(\\) :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
mbig \\ msmall = case remSubset' msmall mbig of
                   Just s -> s
                   Nothing -> empty

members :: Ord a => MultiSet a -> a -> Int
members (MS m) a = M.findWithDefault 0 a m

toList :: Ord a => MultiSet a -> [a]
toList (MS m) = concatMap (\(x,n) -> replicate n x) . M.assocs $ m

