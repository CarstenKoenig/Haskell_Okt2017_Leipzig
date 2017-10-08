---
title: "Haskell für Fortgeschrittene - Aeson"
author: Carsten König
date: Oktober 2017
geometry: margin=2cm
output: pdf_document
papersize: a4
---

# Json Encode/Decode

## Beispiel Events

```haskell
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
```
