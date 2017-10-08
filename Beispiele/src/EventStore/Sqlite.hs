{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module EventStore.Sqlite
  ( runSqlitePool
  , testComputation
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Pool
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH

import EventStore.Class

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Event
    aggregateKey Int
    aggregateOrder Int
    jsonData ByteString
    UniqueAggregateIndex aggregateKey aggregateOrder
    deriving Show
|]


newtype SqlStoreMonad ev a = SqlStoreMonad (ReaderT SqlBackend (LoggingT IO) a)
  deriving (Functor, Applicative, Monad)


runSqlitePool :: Text -> Int -> SqlStoreMonad ev a -> IO a
runSqlitePool conStr poolSize (SqlStoreMonad m) =
  runStderrLoggingT $ withSqlitePool conStr poolSize $ \pool ->
    flip runSqlPool pool $ do
      runMigration migrateAll
      m


instance MonadIO (SqlStoreMonad ev) where
  liftIO = SqlStoreMonad . liftIO


instance (FromJSON ev, ToJSON ev) => EventStoreMonad ev (SqlStoreMonad ev) where

  newAggregate = SqlStoreMonad $ do
    newKey <- maybe 1 (+ 1) . listToMaybe . map (eventAggregateKey . entityVal)
              <$> selectList [] [Desc EventAggregateKey, LimitTo 1]
    insert $ Event newKey 0 ""
    return newKey


  addEvent key ev = SqlStoreMonad $ do
    newInd <- maybe 1 (+ 1) . listToMaybe . map (eventAggregateOrder . entityVal)
              <$> selectList [ EventAggregateKey ==. key ] [Desc EventAggregateOrder, LimitTo 1]
    insert $ Event key newInd $ LBS.toStrict $ encode ev
    return ()

  getEvents key = SqlStoreMonad $
    mapMaybe (decode . LBS.fromStrict . eventJsonData . entityVal)
           <$> selectList [ EventAggregateKey ==. key ] [ Asc EventAggregateOrder ]


----------------------------------------------------------------------

testComputation :: EventStoreMonad String m => m (Int, [String])
testComputation = do
  key <- newAggregate
  addEvent key "Hallo"
  addEvent key "World"
  evs <- getEvents key
  return (key, evs)
