{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE InstanceSigs#-}
import Data.Monoid
class Applicative f => EskoMonad f where
    join :: f (f a) -> f a -- Cf. concat [[a]] -> [a]

instance EskoMonad Maybe where
  join (Nothing) = Nothing
  join (Just(Nothing)) = Nothing
  join (Just (Just a)) = Just a

instance (Monoid a) => EskoMonad ((,) a) where
  join ((,) a ((,) a' b)) = (a<>a,b)


 
