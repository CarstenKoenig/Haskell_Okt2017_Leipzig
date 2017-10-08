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


getResult :: Foldable f => Projection ev a -> f ev -> a
getResult (MkProj init fold final) =
  final . foldl' fold init


lastP :: (ev -> Maybe a) -> Projection ev (Maybe a)
lastP sel = MkProj Nothing fold id
  where fold s ev = sel ev <|> s


projectP :: (event -> b) -> Projection event [b]
projectP p = MkProj [] update reverse
  where
    update xs ev = p ev : xs


collectP :: (event -> Maybe a) -> Projection event [a]
collectP select = catMaybes <$> projectP select


liftP :: (e1 -> Maybe e0) -> Projection e0 r -> Projection e1 r
liftP proj (MkProj i fd fi) =
  let fP s ev =
        case proj ev of
          Just ev' -> fd s ev'
          Nothing  -> s
  in MkProj i fP fi


zipP :: Projection ev a -> Projection ev b -> Projection ev (a,b)
zipP (MkProj ia fda fia) (MkProj ib fdb fib) =
  MkProj (ia,ib) fold (fia *** fib)
  where
    fold (sa,sb) ev = (fda sa ev, fdb sb ev)
