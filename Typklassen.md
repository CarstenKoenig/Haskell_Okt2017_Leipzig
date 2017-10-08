---
title: "Haskell für Fortgeschrittene - Typklassen"
author: Carsten König
date: Oktober 2017
geometry: margin=2cm
output: pdf_document
papersize: a4
---

# Funktoren
## Intuition:

- *Kontainer* mit einem oder mehr Werte
- *Berechnung* die einen Wert produziert
- `fmap` bringt eine Funktion, die Werte transformiert in den Funktor

## Typklasse

```haskell
:i Functor
class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
    {- MINIMAL fmap #-}
```

`<$>` ist Operator für `fmap`

## Laws

- `fmap id = id`
- `fmap (g . h) = (fmap g) . (fmap h)`


## Beispiele

- Listen
- `Maybe`
- `IO`
- `Either a`
- `((->) r)`
- `((,) a)`

```haskel
> fmap succ [1..5]
[2,3,4,5,6]

> fmap succ (Just 5)
Just 6

> fmap succ Nothing
Nothing

> fmap (++ "!") getLine
Hello
"Hello!"

> fmap succ (Right 5)
Right 6

> fmap succ (Left "argh")
Left "argh"

> fmap succ (*5) 2
11

> fmap succ ("Hallo", 41)
("Hallo",42)
```

## Extension
mit `{- LANGUAGE DeriveFunctor #-}` kann fast jeder ADT zu einem Funktor gemacht werden (mittels `deriving Functor`)

## Übungen

- Functor Instanz für `data Pair a = Pair a a`
- Functor Instanz für `data ITree a = Leaf (Int -> a) | Node [ITree a]`
- Gesucht Typ mit Kind `* -> *`, der kein Functor ist

## Functor sind sind *komposierbar*

### Komposition
```haskell
newtype Compose f g a = Compose (f (g a))
  deriving Show


instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose $ fmap (fmap f) x


type MaybeList a = Compose [] Maybe a

example :: MaybeList String
example = fmap show $ Compose [Just 5, Nothing, Just 4]
```

### Coprodukt
```haskell
newtype Coproduct f g a = Coproduct (Either (f a) (g a))
  deriving Show


instance (Functor f, Functor g) => Functor (Coproduct f g) where
  fmap f (Coproduct (Left x)) = Coproduct (Left $ fmap f x)
  fmap f (Coproduct (Right y)) = Coproduct (Right $ fmap f y)


type MaybeOrList a = Coproduct Maybe [] a

exampleL :: MaybeOrList String
exampleL = fmap show $ Coproduct (Left $ Just 5)

exampleR :: MaybeOrList String
exampleR = fmap show $ Coproduct (Right $ [7])
```

## *exotische* Funktoren

```haskell
newtype Ident a = Ident { runId :: a }

instance Functor Ident where
  fmap f (Ident a) = Ident (f a)


newtype Const b a = Const { runConst :: b }

instance Functor (Const b) where
  fmap f (Const b) = Const b
```


## Einschub: `fix`

```haskell
fix :: (a -> a) -> a
```

Beispiele:

```haskell
fix (1:) == [1,1,1,..]
fix (\c x -> if x <= 1 then 1 else x * c (x-1)) 5 == 120
```

Für Typen:

```haskell
newtype Fix f = Fix { unFix :: f (Fix f) }
```


## heftiges Beispiel Recursion Schemes

```haskell
{- LANGUAGE DeriveFunctor #-}

module RecSchemes where


data ExprF a
  = ValF Int
  | AddF a a
  | MulF a a
  deriving (Show, Eq, Functor)


type Expr = Fix ExprF


data Fix f = Fix { unFix :: f (Fix f) }


val :: Int -> Expr
val n = Fix $ ValF n

add :: Expr -> Expr -> Expr
add a b = Fix $ AddF a b

mul :: Expr -> Expr -> Expr
mul a b = Fix $ MulF a b


type Algebra f a = f a -> a


evalAlg :: Algebra ExprF Int
evalAlg (ValF n) = n
evalAlg (AddF a b) = a + b
evalAlg (MulF a b) = a * b


eval :: Expr -> Int
eval = cata evalAlg


cata :: Functor f => Algebra f a -> Fix f -> a
cata alg =
  alg . fmap (cata alg) . unFix


showAlg :: Algebra ExprF String
showAlg (ValF n) = show n
showAlg (AddF a b) = "(" ++ a ++ " + " ++ b ++ ")"
showAlg (MulF a b) = a ++ " * " ++ b


showExpr :: Expr -> String
showExpr = cata showAlg
```


# Applicative Funktoren

## Intuition
was, wenn die Funktion, die wir anwenden wollen, selbst
im *Kontext* *verpackt ist*?

mit `Applicative` können *verpackte* Funktionen auf *verpackte* Werte anwenden

## Typklasse

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

## Laws

- Identity: `pure id <*> v = v`
- Homomorphism: `pure f <*> pure x = pure (f x)`
- Interchange: `u <*> pure y = pure ($ y) <*> u` (die Reihenfolge der Auswertung ist egal)
- Composition: `u <*> (v <*> w) = pure (.) <*> u <*> v <*> w` (ein wenig wie Assoziativität)

mit Funktor:

- `g <$> x = pure g <*> x`


## Beispiele

### Maybe

```haskell
> Just (+) <*> Just 5 <*> Just 6
Just 11

> Just (+) <*> Nothing <*> Just 6
Nothing

> Nothing <*> Just 5
Nothing
```

### Listen

Es gibt zwei Möglichkeiten Listen zu einer Instanz von `Applicative` zu machen:

- Funktionen-Liste und Werte-Liste paarzweise anwenden
- jede Funktion in der Funktionen-Liste mit jedem Wert in der Werte-Liste applizieren

---

#### Übung
Newtypes für die beiden Möglichkeiten und Applicative implementieren

##### Lösung
```haskell
newtype ZipList a = ZipList [a]
  deriving (Show, Functor)

instance Applicative ZipList where
  pure = ZipList . repeat
  ZipList fs <*> ZipList xs = ZipList $ zipWith ($) fs xs


newtype CompList a = CompList [a]
  deriving (Show, Functor)

instance Applicative CompList where
  pure a = CompList [a]
  CompList fs <*> CompList xs =
    CompList $ [ f x | f <- fs, x <- xs ]
```

---

### Paare
Wie könnte eine Instanz für `(,) a` aussehen?

- Für `pure x :: x -> (a,x)` brauchen wir einen Wert für `a0 :: a`
- Für `(a1,f) <*> (a2,x) = (a1 ? a2, f x)` müssen wir uns etwas für `? :: a -> a -> a` ausdenken
   - `pure id <*> (a,x) = (a0, id) <*> (a0 ? a x) =(law)= (a, x)` - d.h. `a0 ? a = a`
   - analog wegen interchange für `a ? a0 = a`
   - Composition schließlich heißt `a1 ? (a2 ? a3) = (a1 ? a2) ? a3`
   - eine derartige Operation heißt **Monoid** und dafür gibt es bereits eine Typklasse

```haskell
newtype Paar a b = Paar (a, b)
  deriving (Show, Functor)

instance Monoid a => Applicative (Paar a) where
  pure a = Paar (mempty, a)
  Paar (a,f) <*> Paar (b,x) = Paar (a `mappend` b, f x)
```

---

### Übung
Implemntiere `sequenceAL :: Applicative f => [f a] -> f [a]`

#### Lösung
```haskell
sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL = foldr (liftA2 (:)) (pure [])
```

---

## heftiges Beispiel: Projektionen

```haskell
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module EventStore.Projections
  ( Projection
  , getResult
  , liftP
  , lastP, collectP, projectP
  )
where

import Data.Maybe (catMaybes)
import Data.List (foldl')
import Control.Applicative ((<|>))
import Control.Arrow ((***))


data Projection ev a =
  forall s . MkProj
  { state :: s
  , fold :: s -> ev -> s
  , final :: s -> a
  }


instance Functor (Projection ev) where
  fmap f (MkProj s fd fi) = MkProj s fd (f . fi)


instance Applicative (Projection ev) where
  pure a = MkProj () const (const a)
  pF <*> pX = uncurry ($) <$> zipP pF pX


zipP :: Projection ev a -> Projection ev b -> Projection ev (a,b)
zipP (MkProj ia fda fia) (MkProj ib fdb fib) =
  MkProj (ia,ib) fold (fia *** fib)
  where
    fold (sa,sb) ev = (fda sa ev, fdb sb ev)


getResult :: Projection ev a -> [ev] -> a
getResult (MkProj init fold final) =
  final . foldl' fold init


lastP :: (ev -> Maybe a) -> Projection ev (Maybe a)
lastP sel = MkProj Nothing fold id
  where fold s ev = sel ev <|> s
```

### Verwendung
```haskell
import Data.Functor.Compose (Compose(..))


project :: EventStoreMonad ev m => Projection ev a -> Int -> m a
project p key =
  getResult p <$> getEvents key


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
```
