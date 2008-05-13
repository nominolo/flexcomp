{-# OPTIONS -fglasgow-exts #-}
module GraphMap where

import Prelude hiding ( null )
import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ( foldl' )
import Data.Maybe ( isJust )
import Control.Monad ( liftM )

import Test.QuickCheck hiding ( label )

import PPrint

------------------------------------------------------------------------------

newtype GraphMap n l = GM (Map n (Map n l)) deriving (Eq, Show)

instance (PPrint n, PPrint l) => PPrint (GraphMap n l) where
    pshow (GM m) = pshow m

instance (Ord n, Arbitrary n, Arbitrary l) => Arbitrary (GraphMap n l) where
    arbitrary = liftM fromEdges arbitrary

empty :: GraphMap n l
empty = GM M.empty

null :: GraphMap n l -> Bool
null (GM m) = M.null m

addEdge :: Ord n => (n, l, n) -> GraphMap n l -> GraphMap n l
addEdge (n, l, n') (GM m) = GM $ add'
  where
    add'  = M.alter add_to_n's n m
    add_to_n's Nothing   = Just $ M.singleton n' l
    add_to_n's (Just m') = Just $ M.insert n' l m'

deleteEdge :: Ord n => n -> n -> GraphMap n l -> GraphMap n l
deleteEdge n n' (GM m) = GM $ del'
  where
    del' = M.alter del_from_n's n m
    del_from_n's Nothing = Nothing
    del_from_n's (Just m') = 
        let m'' = M.delete n' m' in
        if M.null m'' then Nothing else Just m''
                       

fromEdges :: Ord n => [(n, l, n)] -> GraphMap n l
fromEdges = foldl' (flip addEdge) empty

fromEdgesWith :: Ord n => (Maybe l' -> l -> l') -> [(n, l, n)] -> GraphMap n l'
fromEdgesWith comb = foldl' comb' empty
  where comb' g (n, l, n') = modifyLabel n n' (mod l) g
        mod l ml' = Just (comb ml' l)
{-
t = fromEdgesWith add2set [(1,'a',3),(1,'b',3),(2,'c',3)]
  where add2set Nothing x = S.singleton x
        add2set (Just s) x = S.insert x s
-}
edgeLabel :: Ord n => n -> n -> GraphMap n l -> Maybe l
edgeLabel n n' (GM m) = 
    M.lookup n m >>= M.lookup n'

hasEdge :: Ord n => n -> n -> GraphMap n l -> Bool
hasEdge n n' = isJust . edgeLabel n n'

-- XXX: could be implemented slightly more efficiently
modifyLabel :: Ord n => n -> n -> (Maybe l -> Maybe l) -> GraphMap n l 
            -> GraphMap n l
modifyLabel n n' f g = case f (edgeLabel n n' g) of
  Nothing -> deleteEdge n n' g
  Just l -> addEdge (n, l, n') g

transpose :: Ord n => GraphMap n l -> GraphMap n l
transpose (GM m) = M.foldWithKey inv empty m
  where
    inv n m' mm = M.foldWithKey inv' mm m'
      where inv' n' l mm = addEdge (n', l, n) mm

successors :: Ord n => n -> GraphMap n l -> Set n
successors n (GM m) = M.keysSet $ M.findWithDefault M.empty n m

outEdges :: Ord n => n -> GraphMap n l -> Map n l
outEdges n (GM m) = M.findWithDefault M.empty n m

edges :: GraphMap n l -> [(n, l, n)]
edges (GM m) 
    = [ (n, l, n') | (n, n's) <- M.assocs m, (n',l) <- M.assocs n's ]

-- | Return all nodes in the graph in ascending order.
nodes :: Ord n => GraphMap n l -> [n]
nodes (GM m) = S.toList $ M.fold uni (M.keysSet m) m
  where uni = S.union . M.keysSet

------------------------------------------------------------------------------

prop_addNonEmpty x y z = not $ null $ addEdge (x::Int,y,z) $ empty 

prop_addEdge x l y g = 
    edgeLabel x y (addEdge (x::Int,l::Int,y) g) == Just l

prop_transpose x y g = 
    edgeLabel x y g == edgeLabel y x (transpose g :: GraphMap Int Int)

prop_transposeNodes g =
    nodes (g :: GraphMap Int Int) == nodes (transpose g)
      
