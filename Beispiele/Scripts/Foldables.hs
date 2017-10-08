{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foldables where

import Control.Monad (join)
import Data.Foldable

newtype Endo a = Endo { appEndo :: a -> a }


instance Monoid (Endo a) where
  mempty                  = Endo id
  Endo f `mappend` Endo g = Endo (f . g)


foldR :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldR f b = flip appEndo b . foldMap (Endo . f)


foldMap' :: (Foldable t, Monoid m, Monad t) => (a -> t m) -> t a -> m
foldMap' f as = fold $ join (fmap f as)
