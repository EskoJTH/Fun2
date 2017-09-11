import Data.Monoid hiding (Sum, First, Last, Endo, Product, Min, Max, NonEmpty,Endo,Predicate)
import Data.Semigroup hiding (Sum, First, Last, Endo, Product, Min, Max, NonEmpty, Endo,Predicate)
import Data.Map
--a
newtype Count = Count {getCount :: Integer} deriving (Eq,Show)
instance Monoid Count where
  mempty = Count 0
  mappend (Count a) (Count b) =  Count (a + b)
--(Count a <> Count b) <> Count c == Count (a + b) <> Count c == Count ((a + b) + c) Count (a + b + c) == Count (a + (b + c))
-- ==Count a <> Count(a + b) == Count a <> (Count a <> Count  b)
--(Count 0) <> (Count a) == Count (a + 0) == Count a
--mempty triviaalisti sama topisinpain
--b
newtype Sum a = Sum {getSum :: a} deriving (Eq,Show)
instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum c) (Sum b)  =  Sum (c + b)
--Triviaalisti sama kuin aikaisempi
--c
newtype Product a = Product {getProfuct :: a} deriving (Eq,Show)
instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product a) (Product b) = Product (a * b)
--kertolasku saannot ovat samankaltaisia kuin yhteenlasku mutta mempty pitaa kasitella
--(Product 1) <> (Product a) == Product (a * 1) ==  Product a
--mempty Triviaalisti sama toisinpain
--d
newtype First a = First {getFirst :: Maybe a} deriving (Eq, Show)
instance Monoid (First a) where
  mempty = First Nothing
  mappend (First (Just a)) (First (Nothing)) = First (Just a)
  mappend (First (Nothing)) (First(Just b)) = First (Just b)
  mappend (First(Just a)) (First(Just b)) = First (Just a)
--(First (Just a) <> First (Just b)) <> First (Just c) == First (Just a) <> First (Just c) == First (Just a)
--First (Just a) <> (First (Just b) <> First (Just c)) ==First (Just a) <> First (Just b) == First (Just a)
--First (Just a) <> First Nothing == First (Just a)
--First (Nothing) <> First (Just a) == First (Just a)
--e
newtype Last a = Last {getLast :: Maybe a} deriving (Eq, Show)
instance Monoid (Last a) where
  mempty = Last Nothing
  mappend (Last(Just a))(Last(Nothing)) = Last (Just a)
  mappend (Last(Nothing))(Last(Just b)) = Last (Just b)
  mappend (Last(Just a))(Last(Just b)) = Last (Just b)
--Triviaalisti sama kuin aikaisempi mutta toimii toisin pain.
--
  
--f
newtype Min a = Min {getMin :: a} deriving (Ord,Show,Eq)
instance Ord a => Semigroup (Min a) where
  (<>) (Min a) (Min b) 
    |a<b = Min a
    |a>b = Min b
    |otherwise = Min a
--Etsii aina alkioista pienimm‰n joten jarjestyksella ei ole valia
--g
newtype Max a = Max {getMax :: a} deriving (Ord,Show,Eq)
instance Ord a => Semigroup (Max a) where
  (<>) (Max b) (Max a) 
    |a<b = Max b
    |a>b = Max a
    |otherwise = Max a
--Triviaalisti sama kuin aikaisempi
--h
data NonEmpty a = a :| [a] deriving (Eq, Show)
instance Semigroup (NonEmpty a) where
  (<>) (a :| as) (b :| bs) = a :| (as ++ (b : bs))
--Ei vaihda alkioiden jarjestysta listassa missaan kohtaa joten operaatioiden jarjestyksella ei valia
--i
newtype MonMap k a = Mon (Map k a) deriving (Show)
instance (Ord k, Monoid m) => Monoid (MonMap k m) where
    mempty = Mon (Data.Map.empty)
    mappend (Mon m1) (Mon m2) = Mon (Data.Map.unionWith mappend m1 m2)

--Data.Map.UnionWith jarjestaa Data.Map:it Avainten mukaiseen jarjestykseen.
--Talloin Data.Map:in jarjestys on aina sama kun kaikki avaimet on k‰sitelty.
--j
newtype Predicate a = Pred (a -> Bool)
instance Semigroup (Predicate a) where
  (<>) (Pred a) (Pred b) = Pred (\x->(a x) || (b x))
--OR operaation on assosiatiivinen  
--k
newtype Endo a = Endo {appEndo :: a -> a}
instance Monoid (Endo a) where
  mempty = Endo (\a->a)
  mappend (Endo a) (Endo b)  = Endo (a . b)
--(.) f g = \x -> f (g x)
--(Endo a <> Endo b) <> Endo c == Endo (a . b) <> Endo c = Endo ((a . b) . c)
-- == Endo (\x -> a (b x)) . c == Endo (\x -> a (b (c x)))
--Endo a <> (Endo b <> Endo c) == Endo a <> Endo (b . c) = Endo (a . (b . c))
-- == Endo (a . (\x -> b (c x))) == Endo (\x -> a (b (c x)))
-- mepty ei triviaalisti muuta mit‰‰n.
--l
newtype Fun a b = Fun {getFun :: a->b}
instance (Monoid b) => Semigroup (Fun a b) where
  (<>) (Fun c) (Fun d) = Fun (\x->(c x) Data.Monoid.<> (d x))
--En erikseen merkitse k‰yt‰nkˆ Data.Monoid vai Data.Semigroup Koska sen pit‰isi olla itsest‰‰n selv‰‰.
--(Fun a <> Fun b) <> Fun c == Fun (\x -> (a x) <> (b x)) <> Fun c = Fun (\x -> ((a x) <> (b x)) <> (c x))
--(<>) on assosiatiivinen eli
--Fun (\x -> (a x) <> (b x) <> (c x)) == Fun (\x -> (a x) <> ((b x) <> (c x))) == Fun a <> Fun (\x -> (b x) <> (c x))
--  == Fun a <> (Fun b <> Fun c)

--m
newtype Pair a b = Pair {getPair :: (a,b)} deriving (Show)
instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair (mempty,mempty)
  mappend (Pair (a,b)) (Pair (c,d)) = Pair (mappend a c, mappend b d)
--(Pair (a,b) <> Pair (c,d)) <> Pair (e,f) ==Pair (a <> c, b <> d), Pair (e,f) == Pair (a <> c <> e, b <> d <> f)
-- == Pair (a,b) <> Pair (c <> e, d <> f) == Pair (a,b) <> (Pair (c,d) <> Pair (e,f))
--toiminta memptyn kanssa on triviaalisti tosi.
--n
newtype Kissa a = Kissa {getKissa :: IO a}
instance (Monoid a) =>Monoid (Kissa a) where
  mempty = Kissa (return (mempty))
  mappend (Kissa a) (Kissa b) = Kissa (a >> b)
--Puoliryhm‰n j‰senten j‰rjestys ei miss‰‰nkohtaa muutu
--Tyhj‰n palauttaminen vain palauttaa tyhj‰n eik‰ tee mit‰‰n.

