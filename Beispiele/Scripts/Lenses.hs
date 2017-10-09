{-# LANGUAGE RankNTypes #-}

module Lenses where

import Data.Functor.Identity
import Data.Functor.Const

-- ein paar Records

data Name = Name
  { vorname :: String
  , nachname :: String
  } deriving Show


beispielName :: Name
beispielName = Name "Max" "Muster"


data Person = Person
  { name :: Name
  , email :: String
  } deriving Show


maxMuster :: Person
maxMuster = Person (Name "Max" "Muster") "max.muster@mail.me"



----------------------------------------------------------------------
-- naive Implementation

data Lens1 s a = Lens1
  { view1 :: s -> a
  , set1  :: a -> s -> s
  }

vornameLens1 :: Lens1 Name String
vornameLens1 = Lens1 vorname (\x n -> n { vorname = x })

nameLens1 :: Lens1 Person Name
nameLens1 = Lens1 name (\x p -> p { name = x })

setzeVorname1 :: String -> Person -> Person
setzeVorname1 n p =
  let alterName = view1 nameLens1 p
      neuerName =
        set1 vornameLens1 n alterName
  in set1 nameLens1 neuerName p

-- Versuch 2 mit `over`:

data Lens2 s a = Lens2
  { view2 :: s -> a
  , over2 :: (a -> a) -> s -> s
  }


set2 :: Lens2 s a -> a -> s -> s
set2 l a s =
  over2 l (const a) s

vornameLens2 :: Lens2 Name String
vornameLens2 = Lens2 vorname (\f n -> n { vorname = f $ vorname n })

nameLens2 :: Lens2 Person Name
nameLens2 = Lens2 name (\f p -> p { name = f $ name p })

setzeVorname2 :: String -> Person -> Person
setzeVorname2 n p =
  over2 nameLens2 (set2 vornameLens2 n) p

-- mit Seiteneffekten:

data Lens3 s a = Lens3
  { view3 :: s -> a
  , over3 :: (a -> a) -> s -> s
  , overIO3 :: (a -> IO a) -> s -> IO s
  }


----------------------------------------------------------------------
-- van Laarhoven lens

type VLLens s a =
  forall f . Functor f => (a -> f a) -> s -> f s


-- we can get over with the Identity Functor
overVL :: VLLens s a -> (a -> a) -> s -> s
overVL l f s = runIdentity $ l (Identity . f) s


-- and view with the Const Functor!
viewVL :: VLLens s a -> s -> a
viewVL l s = getConst $ l Const s


setVL :: VLLens s a -> a -> s -> s
setVL l a = overVL l (const a)


vornameLensVL :: VLLens Name String
vornameLensVL f n =
  fmap (\v -> n { vorname = v }) (f $ vorname n)


nameLensVL :: VLLens Person Name
nameLensVL f p =
  fmap (\n -> p { name = n }) (f $ name p)


-- Komposition direkt mit `.` (umgekehrt!)
personVorname :: VLLens Person String
personVorname = nameLensVL . vornameLensVL


setVorname :: String -> Person -> Person
setVorname = setVL personVorname

getVorname :: Person -> String
getVorname = viewVL personVorname


