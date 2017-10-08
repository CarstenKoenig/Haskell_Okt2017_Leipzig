{-# LANGUAGE DeriveGeneric #-}
module Events where

import Data.Aeson
import GHC.Generics

data Person
  = NameGesetzt String
  | VornameGesetzt String
  | AlterGesetzt Int
  deriving (Show, Generic)


instance ToJSON Person
instance FromJSON Person
