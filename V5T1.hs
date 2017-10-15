{-#language TypeSynonymInstances#-}
{-#language FlexibleInstances#-}
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
{-
data UFOLanguage next =
  Move Point next |
  Relax next |
  ScanPlayer (Point -> next) |
  Shoot Point next
-}

runUfoSimulation = snd $ runWriter $ runReaderT (playRound::T()) []

playRound :: UFOLang m => m ()
--playRound :: T ()
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

