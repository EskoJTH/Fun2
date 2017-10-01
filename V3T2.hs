{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE InstanceSigs#-}
--a
--Applicative has (<*>) :: f (a -> b) -> f a -> f b
--Applicative has pure :: a -> f a
class Applicative f => EskoMonad f where
      (>>>=) :: f a -> (a -> f b) -> f b
      join :: f (f a) -> f a -- Cf. concat [[a]] -> [a]
      return :: a -> f a
  
{-    
join :: (EskoMonad f) => f (f a) -> f a
join a = a >>>= (\x->x)
-}

{-
(>>=) :: (EskoMonad f) => f a -> (a -> f b) -> f b
(>>=) fa afb= join (fmap afb fa)
-}

--b
mfmap :: Monad m => (a -> b) -> m a -> m b
mfmap f ma = (pure f) <*> ma

{-

FunctorLaws:
fmap id  ==  id
fmap (g . g')  ==  fmap g . fmap g'

LawsOfApplicators
pure f <*> pure x = pure (f x)

(.) f g = \x -> f (g x)

proof: 
mfmap (\x->x) = \(f a)->(pure (\x->x)) <*> f a)
--pure :: a -> f a pure lifts an value.
= (\(f a)->(f (\x->x)) <*> f a)
--(f (\x->x)) <*> f a = f a
= f a -> f a
--id funktio palauttaa saman asian kuin ottaa sis‰‰ns‰. t‰m‰ tekee samoin. Tosin sen t‰ytyy olla tyyppi‰ f a jossa f on Monad.

mfmap (g . g')
----
= (\(f a)->(pure (g . g')) <*> (f b))

----(.) f g = \x -> f (g x)
= (\(f a)->(pure (\x->g(g' x))) <*> (f a))

----pure :: a -> f a pure lifts an value.
= (\(f a)->(f (\x->g(g' x))) <*> (f a))

----pure f <*> pure x = pure (f x)
= (\(f a)->(f ((\x->g(g' x))) a))

----(\x->g(g')) otetaan parametri sis‰‰n.
= (\(f a)->(f (g(g' a))))

----Tarkastetaan mihin p‰‰dyt‰‰n jos sievennet‰‰n toinen p‰‰ty.

mfmap g . mfmap g'
----mfmap f ma = (pure f) <*> ma
= (\(f a)->(f g)<*>(f a)) . (\(f b)->(f g')<*>(f b))

----pure f <*> pure x = pure (f x)
= (\(f a)->(f (g a))) . (\(f b)->(f (g' b)))

----(.) f g = \x -> f (g x) //oletetaan ett‰ kumpikin sis‰lt‰‰ samantyyppisi‰ monadeja.
= (\(f b)->(f (g (g' b))))

----Lopputulokset samoja koska a ja b ovat molemmat avoimia tyyppej‰. T‰llˆin p‰tee ett‰:
----mfmap g . mfmap g' == mfmap (g . g')


----(.) f g = \x -> f (g x)
= mfmap (\x-> g(mfmap g' x))
----mfmap f ma = (pure f) <*> ma
= \(f a)->(f (\x-> g(mfmap g' x))) <*> (f a))

----pure f <*> pure x = pure (f x)
= \(f a)->(f (g(mfmap g' a)))

----mfmap f ma = (pure f) <*> ma
= \(f a)->(f (g((f g') a)))

----pure f <*> pure x = pure (f x)
= \(f a)->(f (g(\(f b)->(f (g' b)))) a)
-}
