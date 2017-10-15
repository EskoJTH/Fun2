{-#language TypeSynonymInstances#-}
{-#language FlexibleInstances#-}
{-#language DeriveFunctor#-}
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Control.Monad.Free

{-
*Main>  (\ pt -> runWriter (runReaderT (playRound :: T ()) pt)) [""]
((),"scannattiin, tulitettiin pistetta (0,0), liikuttiin pisteeseen (1,1), ")
-}

playRound :: UFOLang m => m ()
playRound = do
  relax
  target <- scanTargets
  shoot target
  move (1,1)

class Monad m => UFOLang m where
  move  :: (Int,Int) -> m() 
  relax :: m()
  scanTargets :: m(Int,Int) 
  shoot :: (Int,Int) -> m()
  
type T = ReaderT PeliTila (Writer String)

type PeliTila = [String]

data UFOLanguage next =
  Move Point next |
  Relax next |
  ScanPlayer (Point -> next) |
  Shoot Point next deriving Functor

{-
instance Functor UFOLanguage where
  fmap f g = case g of
      Move (Point (a,b)) next -> f next
      Relax next -> f next
      ScanPlayer fnext -> _ -- (\p -> f (fnext p))
      Shoot (Point (a,b)) next -> f next
-}

newtype Point = Point (Int,Int)

instance UFOLang (Free UFOLanguage) where
  move (a,b) = Free $ Move (Point (a,b)) (Pure ())  -- :: (Int,Int) -> m() 
  relax = Free $ Relax (Pure ())
  scanTargets = Free $ ScanPlayer (\(Point p)-> Pure p)
  shoot (a,b) = Free $ Shoot (Point (a,b)) (Pure ())

instance UFOLang T where
  -- relax :: m()
  relax = return ()
  move (a,b) = do
    p <- ask
    tell $ ("liikuttiin pisteeseen (" ++ show a ++ "," ++ show b ++ "), ")
    return ()
  scanTargets = do
    p <- ask
    tell $ ("scannattiin, ")
    return (0,0)
  shoot (a,b) = do
    p <- ask
    tell $ ("tulitettiin pistetta (" ++ show a ++ "," ++ show b ++ "), ")

runUfoRound = retract (playRound :: Free UFOLanguage ())
--  f = 
