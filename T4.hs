class MyFunctor f where
  fmap :: (a -> b) -> (f a -> f b)

class Contravariant f where
  conmap :: (b -> a) -> (f a -> f b)
  
class BiFunctor f where
  bimap :: (a -> x) -> (b -> y) -> f a b -> f x y

class ProFunctor f where
  promap :: (x -> a) -> (b -> y) -> (f a b -> f x y)

newtype Identity a = Identity {runIdentity :: a}
instance MyFunctor Identity where
  fmap f (Identity a) = (Identity (f a))
  

newtype Predicate a = Pred (a -> Bool)
instance Contravariant Predicate where
  conmap f (Pred a) = (Pred (a . f))


newtype Endo a = Endo {appEndo :: a -> a}
--Ei pysty. koska ei ole mahdollista kasata operaatiota x->a->a->y.

newtype Kissa a b = Kissa {kissaks :: a -> b}
instance ProFunctor (Kissa) where
  promap f1 f2 (Kissa f) = Kissa (f2 . f . f1 )
--Functor
  
newtype Ehka a = Ehka {ehka :: Maybe a}
instance Functor Ehka where
  fmap f (Ehka (a)) = case a of 
    Just x -> Ehka (Just (f x))
    Nothing -> Ehka Nothing
--Jatkuu????

newtype RIO a b = RIO {runRIO :: a -> IO b}
instance ProFunctor (RIO) where
  promap f1 f2 (RIO f) = RIO (f3 . f . f1 ) where
    f3 ioa = do
      a <- ioa
      return (f2 a)

