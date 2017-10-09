{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Control.Lens

-- ein paar Records

data Name = Name
  { _vorname :: String
  , _nachname :: String
  } deriving Show


makeLenses ''Name


beispielName :: Name
beispielName = Name "Max" "Muster"


data Person = Person
  { _name :: Name
  , _email :: String
  } deriving Show


makeLenses ''Person


maxMuster :: Person
maxMuster = Person (Name "Max" "Muster") "max.muster@mail.me"
