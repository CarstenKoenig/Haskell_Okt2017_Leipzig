{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Compose (Compose(..))
import Events
import EventStore
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


data PersonD = PersonD String String Int
  deriving Show


personP :: Projection Person (Maybe PersonD)
personP = getCompose (pure PersonD <*> Compose nameP <*> Compose vornameP <*> Compose alterP)
  where
    nameP = lastP (
      \case NameGesetzt n -> Just n
            _             -> Nothing)
    vornameP = lastP (
      \case VornameGesetzt v -> Just v
            _                -> Nothing)
    alterP = lastP (
      \case AlterGesetzt a -> Just a
            _              -> Nothing)


readEvs :: EventStoreMonad Person m => Int -> m (Maybe PersonD)
readEvs = project personP
