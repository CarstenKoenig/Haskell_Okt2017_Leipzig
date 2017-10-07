{-# LANGUAGE FunctionalDependencies #-}
module EventStore.Class where

class Monad m => EventStoreMonad ev m | m -> ev where
  newAggregate :: m Int
  addEvent :: Int -> ev -> m ()
  getEvents :: Int -> m [ev]
