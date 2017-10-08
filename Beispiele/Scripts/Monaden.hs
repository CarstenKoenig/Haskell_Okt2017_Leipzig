{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Monaden where

----------------------------------------------------------------------
----------------------------------------------------------------------
-- Except - Monade (Fehler)

newtype Except e a = Except { runExcept :: Either e a }
  deriving (Functor, Applicative)


instance Monad (Except e) where
  Except (Left err) >>= _ = Except (Left err)
  Except (Right  a) >>= f = f a


throwError :: e -> Except e a
throwError err = Except (Left err)


catchError :: Except e a -> (e -> Except e a) -> Except e a
catchError m handler =
  case runExcept m of
    Left err -> handler err
    Right  a -> return a


exceptExample :: Int -> Except String Int
exceptExample 0 = return 0
exceptExample 2 = return 1
exceptExample n
  | even n = do
    halfRes <- exceptExample (n `div` 2)
    return $ 1 + halfRes
  | otherwise  = throwError "keine Potenz von 2"


-- catching:
-- runExcept $ exceptExample 33 `catchError` (const $ return (-1))

----------------------------------------------------------------------
-- ExceptT Monaden-Transformator

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
  deriving (Functor)


instance Monad m => Applicative (ExceptT e m) where
  pure = ExceptT . return . Right
  mf <*> mx = mf >>= (\f -> f <$> mx)


instance Monad m => Monad (ExceptT e m) where
  (ExceptT m) >>= f =
    ExceptT $ m >>= (\case (Left err) -> return (Left err)
                           (Right  a) -> runExceptT (f a))


liftE :: Monad m => m a -> ExceptT e m a
liftE m = ExceptT $ Right <$> m


throwErrorT :: Monad m => e -> ExceptT e m a
throwErrorT err = ExceptT (return $ Left err)


catchErrorT :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchErrorT m handler = ExceptT $ do
  res <- runExceptT m
  case res of
    Left err -> runExceptT $ handler err
    Right  a -> return (Right a)


----------------------------------------------------------------------
-- MonadError

class Monad m => MonadError e m | m -> e where
  throw :: e -> m a
  catch :: m a -> (e -> m a) -> m a


instance Monad m => MonadError e (ExceptT e m) where
  throw = throwErrorT
  catch = catchErrorT


----------------------------------------------------------------------
----------------------------------------------------------------------
-- Reader

newtype Reader r a = Reader { runReader :: r -> a }
  deriving (Functor, Applicative)


instance Monad (Reader r) where
  m >>= f =
    Reader $ \r -> runReader (f (runReader m r)) r


ask :: Reader r r
ask = Reader id


reader :: (r -> a) -> Reader r a
reader = Reader


local :: (r -> r) -> Reader r a -> Reader r a
local adj m = Reader $ \r -> runReader m (adj r)


----------------------------------------------------------------------
----------------------------------------------------------------------
-- State

newtype State s a = State { runState :: s -> (a,s) }
  deriving (Functor)


instance Applicative (State s) where
  pure a = State $ \s -> (a,s)
  sf <*> sx = sf >>= (\f -> f <$> sx)


instance Monad (State s) where
  m >>= f = State $ \s ->
    let (a, s') = runState m s
    in runState (f a) s'


get :: State s s
get = State $ \s -> (s,s)


put :: s -> State s ()
put s = State $ \_ -> ((), s)


modify :: (s -> s) -> State s ()
modify adj = State $ \ s -> ((), adj s)


gets :: (s -> a) -> State s a
gets g = State $ \ s -> (g s, s)


evalState :: State s a -> s -> a
evalState m = fst . runState m


execState :: State s a -> s -> s
execState m = snd . runState m
