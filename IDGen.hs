{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances  #-}
module IDGen
  ( IDGen
  , runIDGen
  , IDGenT
  , runIDGenT
  , MonadIDGen
  , genID
  )
where

import Control.Monad.Trans
import Control.Monad.State ( get, put, MonadState, StateT )
import Control.Applicative

------------------------------------------------------------------------------

newtype IDGenT m a = IDGenT { unIDGenT :: Int -> m (a, Int) } 
--    deriving (MonadState s m)
newtype IDGen a = IDGen { unIDGen :: Int -> (a, Int) }

runIDGenT :: Monad m => Int -> IDGenT m a -> m a
runIDGenT seed gen = do
  (a, _) <- unIDGenT gen seed
  return a

runIDGen :: Int -> IDGen a -> a
runIDGen seed gen = fst $ unIDGen gen seed

instance Monad IDGen where
    return x = IDGen $ \next -> (x, next)
    g >>= k = IDGen $ \next -> 
                let (a, next') = unIDGen g next 
                in unIDGen (k a) next'

instance Monad m => Monad (IDGenT m) where
    return x = IDGenT $ \next -> return (x, next)
    g >>= k = IDGenT $ \next -> do
                (a, next') <- unIDGenT g next
                unIDGenT (k a) next'

instance MonadTrans IDGenT where
    lift m = IDGenT $ \i -> do a <- m; return (a, i)

instance MonadState s m => MonadState s (IDGenT m) where
    get = lift get
    put = lift . put

class Monad m => MonadIDGen m where
    genID :: m Int

instance Monad m => MonadIDGen (IDGenT m) where 
    genID = IDGenT $ \next -> let next' = next + 1 in
                              next' `seq` return (next, next')

instance MonadIDGen IDGen where
    genID = IDGen $ \next -> let next' = next + 1 in
                              next' `seq` (next, next')

instance MonadIDGen m => MonadIDGen (StateT s m) where
    genID = lift genID

t = print =<< runIDGenT 4 (do a <- genID; b <- genID; return (a, b))
