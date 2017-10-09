{-# LANGUAGE FunctionalDependencies #-}
module EventStore
  ( module EventStore.Class
  , SqlStoreMonad
  , Projection
  , project
  , liftP, lastP, collectP, projectP
  , runMemory
  , runSqlitePool
  , runWithSqlPool
  ) where


import EventStore.Class
import EventStore.Projections
import EventStore.Sqlite
import EventStore.Memory


project :: EventStoreMonad ev m => Projection ev a -> Int -> m a
project p key =
  getResult p <$> getEvents key
