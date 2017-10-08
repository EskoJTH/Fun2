{-Implement the following data types using Free and show that the results are isomorphic to the original types.-}
--a
import Control.Monad.Free
data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving Show

data SamaTupla a = Sama (a,a) deriving Show

freeTree :: Free SamaTupla a -> Free SamaTupla a -> Free SamaTupla a
freeTree a b = Free (Sama (a,b))
freeTreeLeaf a = Pure a

convertTree puu = case puu of
  Branch a b -> freeTree (convertTree a) (convertTree b)
  Leaf x -> freeTreeLeaf x

invertTree (Free(Sama(a,b))) = Branch first second where
  first = case a of
    Free (Sama _) -> invertTree a
    Pure x -> Leaf x
  second = case b of 
    Free (Sama _) -> invertTree b
    Pure y -> Leaf y

test = invertTree $ convertTree $ Branch (Branch (Leaf "a") (Leaf "b")) (Leaf "c")


--b
--[a]
data A a = a :| A a | Tyhyja
data VapaaTupla a b = Villi (a,b) deriving Show

vapaaLista a b =Free (Villi(a,b))
vapaaEiLista = Pure ()

pyydystaVilliMonadi (Pure ()) = []
yydystaVilliMonadi (Free(Villi(a,b))) = a:p where
  p=case b of
    Free (Villi(x,y)) -> pyydystaVilliMonadi b
    Pure () -> []

lennaVapaaLista [] = Pure ()
lennaVapaaLista (x:xs) = Free(Villi(x,lennaVapaaLista xs))

vapaaTesti = pyydystaVilliMonadi (lennaVapaaLista [1,2,3,4,5,5,6])

villiJaTyhjaTesti = pyydystaVilliMonadi (lennaVapaaLista [])
