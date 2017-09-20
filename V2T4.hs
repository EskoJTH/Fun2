{-class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b-} 

{-
class (Functor f) => Applicative f where
  {- MINIMAL pure, ((<*>) | liftA2) -}
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 f x = (<*>) (fmap f x)
  (<*>) = liftA2 id
  (*>) :: f a -> f b -> f b
  a1 *> a2 = (id <$ a1) <*> a2
  (<*) :: f a -> f b -> f a
  (<*) = liftA2 const
-}

--a
data Ehka a = Pelkka a | Eimittaan
instance Functor Ehka where
  fmap f (Pelkka a) = Pelkka (f a)
instance Applicative Ehka where  
    pure = Pelkka
    (<*>) (Eimittaan) _ = Eimittaan
    (<*>) _ (Eimittaan) = Eimittaan
    (<*>) (Pelkka f) (Pelkka a)= Pelkka (f a)


newtype Tupla a b = T  (a,b)
instance Functor (Tupla a) where
  fmap f (T (a,b)) = T (a,f b)
instance (Monoid a) => Applicative (Tupla a) where
  pure x = T (mempty,x)
  (<*>) (T ((,) u f)) (T ((,) a b)) =  T (a ,(f b))

--c
newtype ZipList a = Z [a]
instance Functor ZipList where
  fmap f (Z t) = Z(map f t)
instance Applicative ZipList where
  pure x = Z (x:[])
  (<*>) (Z []) _ = Z []
  (<*>) _ (Z []) = Z []
  (<*>) (Z (f:fs)) (Z (x:xs)) = Z((f x): (t))where
        t = unType ((Z fs) <*> (Z xs))
        unType (Z x) = x
