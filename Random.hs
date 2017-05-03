{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Random where

import Control.Monad.Identity
import Data.Bifunctor
import System.Random

newtype RandomT m a = RandomT { runRandomT :: StdGen -> m (a,StdGen) }

type MonadRandom = RandomT Identity

runRandom :: MonadRandom a -> StdGen -> (a, StdGen)
runRandom action stdgen = runIdentity $ (runRandomT action) stdgen

instance (Functor m) => Functor (RandomT m) where
  fun `fmap` (RandomT action) =
    RandomT $ \ gen -> first fun <$> action gen

instance (Monad m) => Applicative (RandomT m) where
  (RandomT funA) <*> (RandomT valA) = RandomT $ \ gen -> do
    (fun, gen') <- funA gen
    (val, gen'') <- valA gen'
    return $ (fun val, gen'')
  pure x = RandomT $ \ gen -> pure (x,gen)

randR :: (Applicative f, Random a) => (a,a) -> RandomT f a
randR interval = RandomT $ \ gen -> pure (randomR interval gen)
