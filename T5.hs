  
class MyFunctor f where
  fmap :: (a -> b) -> (f a -> f b)

  
newtype Const c a = Const c
instance MyFunctor (Const c) where
  fmap f (Const a) = Const a

--instance BiFunctor Const where
--  bimap f1 f2(Const a) = Const (f1 a)

newtype Cont r a = Cont {runCont :: (a -> r) -> r}
instance Functor (Cont r) where
  fmap f (Cont s) = Cont g where
    g h = s (h . f)
    --olipa jotenkin aivan valtava hyppy keksiä tämä.
    --En tainnut itse meinata keksiä tätä laskutapaa.
    -- (a->r)
    -- h :: b -> r
    -- f :: a -> b
    -- s :: (a -> r) -> r
    -- _ :: r
    -- g :: (b -> r) -> r

newtype UpStar f a b = UpStar {runUpStar :: a -> f b}
instance (Functor f) => Functor (UpStar f a) where
  fmap f (UpStar s) = UpStar g where
    g h = Prelude.fmap f (s h)
--Hahaa osasimpas 
-- _ :: f b
-- f :: a -> b
-- h :: r
-- g :: r -> f b
-- s :: r -> f a 
--(a -> b) -> (r -> f a) -> (r -> f b)
  
--mitähän ihmettä tässä on tarkoitus tehdä kun en osaa luoda edes datatyyppiä.
--data Coyoneda f a where Coyoneda :: (b -> a) -> f b -> Coyoneda f a
