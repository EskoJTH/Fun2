import Data.Monoid hiding (Endo)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e = g . foldMap (\a->MelkeinEndo (\b->f b a)) where
 g=(\(MelkeinEndo fu)->(fu e))

newtype MelkeinEndo a = MelkeinEndo {appMelkeinEndo :: a -> a}
instance Monoid (MelkeinEndo a) where
  mempty = MelkeinEndo (\a-> a)
  mappend (MelkeinEndo a) (MelkeinEndo b)  = MelkeinEndo (b . a)
