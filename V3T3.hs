{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE InstanceSigs#-}
--1.Produce n random numbers, where n is taken as an argument
--Onnistuu applicativellä koska tiedämme montako randomia haluamme etukäteen.

import Data.Char
import Data.Bool
import Control.Applicative
--import System.Random
import Data.Map.Strict as M

next' :: Int -> (Int, Int)
next' i = let
  (x,y) = (theList i,0) in --next (mkStdGen i) in
  (x,x)

theList i = theList' i list --Tuottaa huonon vähän randomin jos system.Random:ia ei voida käyttää. Käytän tätä koska kotikoneellani on jostain syystä vaikeuksia löytää System.Random.
theList' 0 (s:ss) = s
theList' i (s:ss) = theList' (i-1) ss

list = [2,8,4,6,1,6,2,3,5,0,1,7,6,4,3,2,1,9,2,8,2,6]

data MyRandom g = MyRandom {runR :: Int -> (g, Int)}
instance Functor MyRandom where
  fmap f (MyRandom x) = MyRandom p where
    p s = (f (fst (x s)),snd (x s))

 -- :: f (a -> b) -> f a -> f b
instance Applicative MyRandom where
  pure r = MyRandom (\int -> (r, int))
  (<*>) (MyRandom rf) (MyRandom rr) = MyRandom p where
    p s = ((f a), s2) where
      (f,s1)= (rf s)
      (a,s2) = (rr s1)

nRandomNums n seed = fst (runR (iteroi n) seed)

iteroi 0 = pure []
iteroi n = (:) <$> MyRandom next' <*> iteroi (n - 1)

--2.Roll a die until a six comes up, printing out the rolls and their numbers
--Tarvitaan monoidi koska toiminnan tulost vaikuttaa tuleviin tapahtumiin.
 
instance Monad MyRandom where
--  (>>=) :: m a -> (a -> m b) -> m b
    (>>=) :: MyRandom a -> (a -> MyRandom b) -> MyRandom b
    (>>=) x f = p where
      p = join (fmap f x)
    
join :: MyRandom (MyRandom a) -> MyRandom a
join (MyRandom x) = MyRandom fun where
  fun i = let (MyRandom x2, ii) = x i in x2 ii

untilSix :: MyRandom [Int]
untilSix = do
  rOut <- MyRandom next'
  if rOut == 6
    then pure [rOut]
    else do
    out2 <- untilSix
    pure (rOut : out2)

sixRan s = runR --Todo

--3.Parse a comma-separated list of natural numbers (such as the familiar "0,1,1,3,5,8,13,21,34")
--Applikatiivi riittää koska vastauksien tulos ei vaikuta seuraavien operaatioden määrään.

newtype Parser a = Parser {skaViJobba :: String -> (String, Maybe a)}

instance Functor Parser where
  fmap f (Parser x) = Parser p where
    p s = case x s of
      (s',Just a) -> (s',Just (f a))
      (s',Nothing) -> (s',Nothing)

instance Applicative Parser where
 pure :: a -> Parser a
 pure x = Parser (\ s -> (s,Just x))
 (<*>) (Parser mf) (Parser mx) = Parser p where
   p s = case mf s of
     (s',Just f) -> case mx s' of
       (s'',Just x) -> (s'', Just (f x))
       (s'',Nothing) -> (s'', Nothing)
     (s',Nothing) -> (s', Nothing)

instance Alternative Parser where
  empty = Parser (\s->(s,Nothing))
  (<|>) (Parser a) (Parser b) = Parser fun where
    fun s = case a s of
      (s', Just k) -> a s
      (s', Nothing) -> b s

parser s = fst (skaViJobba (iteroi' s) s)

iteroi' [] = pure []
iteroi' (s) = (:) <$> Parser choose <*> iteroi' (dropWhile isDigit s) <|> Control.Applicative.empty

choose :: String -> (String, Maybe Int)
choose [] = ([], Nothing)
choose s = case (takeWhile isDigit s) of
  [] -> (dropWhile (\x->not(isDigit x)) s, Nothing)
  a -> (dropWhile isDigit s, Just (read a))

strinki = "1,12,3,4,45,6,7"
-- [1,12,3,4,45,6,7]

--4.Look up two given words from a dictionary (of type Map String String) and return a result if both are found. Easy! Maybe?
--Onnistuu Monadilla kätevästi.
--Onnistuu varmaan applikatiivilla kätevämmin.

d :: Map String String 
d = insert "canary berd" "Some sort of berd I guess.." $
    insert "moose" "Like's forests, Dosen't like moose flies" $
    insert "fox" "Lives in a foxhole" $
    singleton "cat" "An animal that does have ears and whiskers and a tail"
    
--newtype Parser a = Parser {skaViJobba :: String -> (String, Maybe a)}



{-
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) x f = p where
      p = joinP (fmap f x)
    
joinP :: Parser (Parser a) -> Parser a
joinP (Parser x) = Parser fun where
  fun s = let (Parser x2, s2) = x s in x2 s2


twoFromMap :: Parser [Int]
twoFromMap s1 s2 = do
  text1 <- Parser findWord s1
  text2 <- Parser findWord s1
  if (text1==Just a || text2  == Just b)
    then return (Just (a,b))
    else do
    return Nothing
-}


