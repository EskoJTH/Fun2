{-Implement the following data types using Free and show that the results are isomorphic to the original types.-}
--a
import Control.Monad.Free
data Tree a = Branch (Tree a) (Tree a) | Leaf a

data SamaTupla a = Sama (a,a)


freeTree a b = Free (Sama (a,b))
freeTreeLeaf a = Pure a

convertTree puu = case puu of
  Branch a b -> freeTree (convertTree a) (convertTree b)
  Leaf x -> freeTreeLeaf x

--invertTree puu = case puu of
  


--b
--[a]
data A a = a :| A a | Tyhyja

--villiLista a =Free Sama(a,villiLista a)
