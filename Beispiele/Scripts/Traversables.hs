{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Traversables where

import Control.Applicative (liftA2)
import Data.Functor.Identity

newtype MyList a = MyList [a]
  deriving (Functor, Foldable, Show, Eq)

instance Traversable MyList where
  traverse _ (MyList []) =
    pure (MyList [])
  traverse f (MyList (x:xs)) =
    liftA2 (\y (MyList ys) -> MyList (y:ys)) (f x) (traverse f (MyList xs))


fmap' :: Traversable t => (a -> b) -> t a -> t b
fmap' f xs = runIdentity $ traverse (Identity . f) xs


foldMap' :: (Monoid m, Traversable t) => (a -> m) -> t a -> m
foldMap' f xs = fst $ traverse (\a -> (f a, ())) xs
