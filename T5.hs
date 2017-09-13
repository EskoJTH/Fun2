
class MyFunctor f where
  fmap :: (a -> b) -> (f a -> f b)

class Contravariant f where
  conmap :: (b -> a) -> (f a -> f b)
  
class BiFunctor f where
  bimap :: (a -> x) -> (b -> y) -> f a b -> f x y

class ProFunctor f where
  promap :: (x -> a) -> (b -> y) -> (f a b -> f x y)
  
newtype Const c a = Const c
instance MyFunctor (Const c) where
  fmap f (Const a) = Const a

instance BiFunctor Const where
  bimap f1 f2(Const a) = Const (f1 a)

newtype Cont r a = Cont {runCont :: (a -> r) -> r}
instance ProFunctor Cont where
  promap f1 f2 (Cont f) =  _ --Cont (f3 . f . f1) where
    {-f3 ioa = do
      a <- ioa
      return (f2 a)-}

newtype UpStar f a b = UpStar {runUpStar :: a -> f b}
--instance (MyFunctor f) => MyFunctor (UpStar f a) where
--  fmap fu (UpStar f) = (UpStar fu f)
  
--data Coyoneda f a where
--  Coyoneda :: (b -> a) -> f b -> Coyoneda f a
