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



# Monoid
## Intuition
Ein Monoid ist eine mathematische Operation auf einer Menge.
Übersetzt in Haskell geht es natürlich eher um eine Funktion/Operation
auf einem (festen) Typ `a`:

    op :: a -> a -> a
    
diese muss ein zwei Regeln erfüllen:

- assoziativ: ``(a `op` b) `op` c == a `op` (b `op` c)``
- gibt ein neutrales Element `e` mit ``a `op` e == e `op` a == a`` für alle `a`

## Typklasse
```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
```

`mconcat` hat eine default-Implementation mit `foldr`:

```haskell
mconcat = foldr mappend mempty
```

das ist da, damit Typen eine effizientere Implementation vornehmen können,
wenn sie wollen (bei Monoids ist es z.B. möglich die Operationen parallel)
durchzuführen.

compile with `stack ghc -- -O2 -threaded Monoids.hs`
and run with either `+RTS -N1 -s` or `+RTS -N2 -s`
`+RTS` (RunTimeSystem - see [here](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html))
- `-N2` says 2 Cores
- `-s` says print stats

### Beispiel Par-Monad
```haskell
import Control.Parallel
import Data.List

-- I've taken this staright from [this great answer](https://stackoverflow.com/a/19119503/76051) on Stack-Overflow

pfold :: (Num a, Enum a) => (a -> a -> a) -> [a] -> a
pfold _ [x] = x
pfold mappend xs  = (ys `par` zs) `pseq` (ys `mappend` zs) where
  len = length xs
  (ys', zs') = splitAt (len `div` 2) xs
  ys = pfold mappend ys'
  zs = pfold mappend zs'

main :: IO ()
main =
  print $ pfold (+) [ foldl' (*) 1 [1..x] | x <- [1..5000] ]
```

## Instanzen

- `[a]` (leere Liste und `++`)
- Zahlen mit Addition und 0 oder Multiplikation und 1
- `All`, `Any` (Bool-Wrapper für `True`/`&&` bzw. `False`/`||` )
- `Endo : a -> a`

---

### Übung
Implementiere Monoid-Instanz für `newtype Endo a = Endo { appEndo :: a -> a }`

#### Lösung
```haskell
newtype Endo a = Endo { appEndo :: a -> a }


instance Monoid (Endo a) where
  mempty                  = Endo id
  Endo f `mappend` Endo g = Endo (f . g)

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

- Für `pure x :: xludwigsstadt mietpreis pro qm -> (a,x)` brauchen wir einen Wert für `a0 :: a`
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
import Data.Foldable (foldl')
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


getResult :: Foldable f => Projection ev a -> f ev -> a
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


# Foldable

## *WTF*s

```haskell
> length (1,2)
1

> sum (4,2)
2
```

## Intuition
bekanntes Aggregieren/Fold Muster

## Typklasse

```haskell
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldMap :: Monoid m => (a -> m) -> t a -> m
```

---

### Übung
implementiere `foldR` nur mit `foldMap`

#### Lösung

```haskell
foldR :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldR f b = flip appEndo b . foldMap (Endo . f)
```

---

### Frage
Was wäre nötig um `foldMap` mit `fold :: Monoid m => t m -> m` zu implementieren?

```haskell
import Control.Monad (join)
import Data.Foldable

foldMap' :: (Foldable t, Monoid m, Monad t) => (a -> t m) -> t a -> m
foldMap' f as = fold $ join (fmap f as)
```

---

## Komposierbar?

```haskell
:t foldMap . foldMap
```


# Traversable

## Typklasse
```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
```

## Intuition
Verallgemeinert den Funktor - jetzt kann die Abbildung Seiteneffekte haben
die durch `traverse` zusammengefasst werden.

Schaut man sich `sequenceA` an, kann man sich das als Möglichkeit vorstellen
Funktoren zu kommutieren (herauszuziehen)

schau `traverse . traverse` an

## Laws

- `traverse Identity = Identity`
- `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`

## Beispiele
### Implementiere für `MyList`

```haskell
newtype MyList a = MyList [a]
  deriving (Functor, Foldable, Show, Eq)

instance Traversable MyList where
  traverse _ (MyList []) =
    pure (MyList [])
  traverse f (MyList (x:xs)) =
    pure (\y (MyList ys) -> MyList (y:ys)) <*> f x <*> traverse f (MyList xs)
```

---

### Übung (hart)
implementiere `foldMap` nur mit Traversable-Methoden

Hinweis: `(,)` hat eine seltsame `Applicative` Instanz

#### Lösung
```haskell
fmap' :: Traversable t => (a -> b) -> t a -> t b
fmap' f xs = runIdentity $ traverse (Identity . f) xs


foldMap' :: (Monoid m, Traversable t) => (a -> m) -> t a -> m
foldMap' f xs = fst $ traverse (\a -> (f a, ())) xs
```

## siehe auch

[the Essence of the Iterator Pattern](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf)
