{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Events
import EventStore.Class
import EventStore.Sqlite
import System.Environment (getArgs)
import Text.Read (readMaybe)


main :: IO ()
main = do
  args <- getArgs
  runSqlitePool "test.db" 1 $ do
    if null args
      then initDb
      else case readMaybe (head args) of
             Nothing -> liftIO $ putStrLn "bitte Key angeben"
             Just key -> do
               evs <- readEvs key
               liftIO $ print evs


initDb :: EventStoreMonad Person m => m ()
initDb = do
  key <- newAggregate
  addEvent key (NameGesetzt "König")
  addEvent key (VornameGesetzt "Carsten")
  addEvent key (AlterGesetzt 37)


readEvs :: EventStoreMonad Person m => Int -> m [Person]
readEvs = getEvents
