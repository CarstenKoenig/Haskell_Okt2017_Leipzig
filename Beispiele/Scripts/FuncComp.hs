module FuncComp where


newtype Compose f g a = Compose (f (g a))
  deriving Show


instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose $ fmap (fmap f) x


type MaybeList a = Compose [] Maybe a

example :: MaybeList String
example = fmap show $ Compose [Just 5, Nothing, Just 4]


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
