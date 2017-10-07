module Functors where

newtype Ident a = Ident { runId :: a }

instance Functor Ident where
  fmap f (Ident a) = Ident (f a)


newtype Const b a = Const { runConst :: b }

instance Functor (Const b) where
  fmap f (Const b) = Const b
