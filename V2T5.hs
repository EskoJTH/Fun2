import Data.Monoid
{-
identity:
pure id <*> v = v
composition:
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
homomorphism:
pure f <*> pure x = pure (f x)
interchange:
u <*> pure y = pure ($ y) <*> u-}

--a
newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x)= Identity (f x)
--todistus
--identity:
{-
pure id <*> v = v
-}
--composition:
{-
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-}
--homomorphism:
{-
pure f <*> pure x = pure (f x)
-}
--interchange:
{-
u <*> pure y = pure ($ y) <*> u
-}

--b
newtype Compose f g a = Compose (f (g a))
instance (Functor g, Functor f) => Functor (Compose f g) where
  fmap f (Compose c) = Compose fmap2 where
    fmap2 = (fmap (fmap f) c)
instance (Applicative g, Applicative f) => Applicative (Compose f g) where
  pure x = Compose (pure(pure x))
  (<*>) (Compose foo) (Compose fga) = Compose (h foo<*> fga) where
    h :: (Applicative f, Applicative g) => f(g(a -> b)) -> f(g a->g b)
    h fo  = fmap (<*>) fo
--todistus
--identity:
{-
pure id <*> v = v
-}
--composition:
{-
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-}
--homomorphism:
{-
pure f <*> pure x = pure (f x)
-}
--interchange:
{-
u <*> pure y = pure ($ y) <*> u
-}
      
--C
newtype State s a = State (s -> (a, s))
instance Functor (State s) where
  fmap f (State fa) = State g where
    g h = (f(fst(fa h)), (snd(fa h)))
instance Applicative (State s) where
  pure a = State (\s->(a,s))
  (<*>) (State ff) (State fs) =  State (\s-> ((fst(ff s)) (fst(fs s)),s))
--todistus
--identity:
{-
pure id <*> v = v
-}
--composition:
{-
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-}
--homomorphism:
{-
pure f <*> pure x = pure (f x)
-}
--interchange:
{-
u <*> pure y = pure ($ y) <*> u
-}
