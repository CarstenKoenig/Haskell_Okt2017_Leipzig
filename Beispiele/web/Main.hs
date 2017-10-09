{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans (lift)
import           Data.Aeson hiding (json)
import           Data.Functor.Compose (Compose(..))
import qualified Data.Text.Lazy as T
import           EventStore
import           Events
import           GHC.Generics
import           Network.HTTP.Types (internalServerError500, badRequest400)
import           Web.Scotty.Trans


-- wir stacken unseren SqlStoreMonad
type MyScottyM a = ScottyT T.Text (SqlStoreMonad Person) a

-- die Action bauch auch auf SqlStoreMonad auf
type MyActionM = ActionT T.Text (SqlStoreMonad Person)

-- damit sparen wir uns ein lift
instance EventStoreMonad Person MyActionM where
  newAggregate = lift newAggregate
  addEvent key = lift . addEvent key
  getEvents    = lift . getEvents


-- definieren der Routen
scottyApp :: MyScottyM ()
scottyApp = do
  get "/" hello
  get "/:key" getPerson
  get "/:key" failure
  post "/add" addPerson
  post "/add" failure


-- Hauptroute sagt nur Hallo
hello :: MyActionM ()
hello = text "Hello"


-- Fehlerhandler (über next)
failure :: MyActionM ()
failure = do
  json $ object [ "error" .= ("Invalid request" :: String) ]
  status badRequest400  


addPerson :: MyActionM ()
addPerson = do
  -- lese Parameter aus
  name <- param "name" `rescue` (const next)
  vorname <- param "vorname" `rescue` (const next)
  alter <- param "alter" `rescue` (const next)
  -- trage Ereignisse ein
  key <- newAggregate
  addEvent key (NameGesetzt name)
  addEvent key (VornameGesetzt vorname)
  addEvent key (AlterGesetzt alter)
  -- Rückgabe
  json $ object [ "message" .= T.concat ["new person with key ", T.pack (show key), " saved" ] ]


getPerson :: MyActionM ()
getPerson = do
  key <- param "key"
  person <- project personP key
  case person of
    Just p -> json p
    Nothing -> do
      json $ object [ "error" .= T.concat ["person ", T.pack (show key), " not found" ] ]
      status internalServerError500


-- lasse die Scotty-Action mit runSqlitePool laufen
-- jeder Request (:: MyActionM ~> EventStoreMonad)
-- wird dann über `runWithSqlPool pool` verarbeitet
main :: IO ()
main = runSqlitePool "Events.db" 5 $ \pool -> do
  liftIO $ scottyT 8000 (runWithSqlPool pool) scottyApp


----------------------------------------------------------------------
-- Beispiel Projektion

data PersonD = PersonD String String Int
  deriving (Show, Generic)


instance ToJSON PersonD


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
