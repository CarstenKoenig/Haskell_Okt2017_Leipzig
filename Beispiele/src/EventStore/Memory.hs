{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module EventStore.Memory
  ( runMemory
  , exampleComp
  ) where

import EventStore.Class

import Control.Monad.State
import Control.Monad.Except

import Data.Map.Strict as Map

newtype MemoryStoreMonad ev a = MemoryStoreMonad (ExceptT String (State (Map.Map Int [ev])) a)
  deriving (Functor, Applicative, Monad, MonadState (Map.Map Int [ev]), MonadError String)


instance EventStoreMonad ev (MemoryStoreMonad ev) where
  newAggregate = do
    key <- (+1) <$> gets Map.size
    modify (Map.insert key [])
    return key

  getEvents key = do
    lookupRes <- gets (Map.lookup key)
    case lookupRes of
      Nothing -> throwError ("Key " ++ show key ++ " nicht im Store")
      Just evs -> return evs

  addEvent key ev =
    modify (Map.insertWith (++) key [ev])


exampleComp :: (MonadError String m, EventStoreMonad String m) => m [String]
exampleComp = do
  key <- newAggregate
  addEvent key "Ev1"
  addEvent key "Ev2"
  throwError "huh"
  getEvents key


runMemory :: Map.Map Int [ev] -> MemoryStoreMonad ev a -> (Either String a, Map Int [ev])
runMemory map (MemoryStoreMonad m )=
  flip runState map $ runExceptT m
