module PPrint 
    ( PPrint
    , pshow
    , pprint
    , pretty
    , Filled(..)
    )
where

import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.PrettyPrint.HughesPJ as PP

------------------------------------------------------------------------------

class PPrint a where
    pshow :: a -> Doc

pretty :: PPrint a => a -> String
pretty = render . pshow

pprint :: PPrint a => a -> IO ()
pprint = putStrLn . pretty

------------------------------------------------------------------------------

instance PPrint a => PPrint (Set a) where
    pshow s = braces $ fsep $ punctuate comma $ map pshow $ Set.toList s

instance PPrint Int where pshow = int 

instance (PPrint a, PPrint b) => PPrint (a, b) where 
    pshow (x,y) = parens $ sep $ punctuate comma $ [pshow x, pshow y]
instance (PPrint a, PPrint b, PPrint c) => PPrint (a, b, c) where 
    pshow (x,y,z) = parens $ sep $ punctuate comma $ [pshow x, pshow y, pshow z]

instance PPrint a => PPrint (Maybe a) where
    pshow Nothing = text "(nothing)"
    pshow (Just a) = pshow a

instance PPrint a => PPrint [a] where
    pshow [] = text "(none)"
    pshow xs = (brackets . sep . punctuate comma . map pshow) xs

instance (PPrint k, PPrint a) => PPrint (Map k a) where
    pshow m = fsep $ map p $ Map.assocs m
      where p (k, a) = pshow k <> colon $$ nest 5 (pshow a)

instance PPrint () where pshow _ = text "()"

instance PPrint Bool where
    pshow = text . show

newtype Filled a = Filled [a] deriving (Eq, Show, Ord, Read)
instance PPrint a => PPrint (Filled a) where
    pshow (Filled []) = text "(none)"
    pshow (Filled xs) = (brackets . fsep . punctuate comma . map pshow) xs
    