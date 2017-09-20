--a

newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = (Identity (f a))

--b
newtype Compose f g a = Compose (f (g a))
instance(Functor g, Functor f) => Functor (Compose f g) where
  fmap f (Compose c) = Compose fmap2 where
    fmap2 = (fmap (fmap f) c)
    --Ei ollu helppo tajuta.

  
newtype State s a = State (s -> (a, s))
instance Functor (State s) where
  fmap f (State fa) = State g where
    g h = (f(fst(fa h)), (snd(fa h)))
    --fa :: s->(a,s)
    --g :: s->(b,s)
    --h :: s
    --f (a->b)
    -- (a->b) -> (s->(a,s) -> s->(b,s))
    --Toimiikohan tämä oikein? Taisin osata nyt pitäis toimia oikein.
