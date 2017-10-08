{-# LANGUAGE DeriveFunctor #-}

module SimpleEffectLanguage where
import Control.Monad.Free -- stakki hoitaa.

data Terminal next =
  SlurpLine (String -> next) |
  BarfLine String next |
  Done
  deriving Functor

slurpLine :: Free Terminal String --as the counterpart of getLine,
slurpLine = Free (SlurpLine (pure)) -- mistä tuo pure tulee? Tai taisin jo keksiä. --Kysy tästä!
barfLine :: String -> Free Terminal () --as the counterpart of putStrLn and
barfLine s = Free (BarfLine s (terminate))
terminate :: Free Terminal () --as the counterpart of pure ()
terminate = Free Done

kaikuupi = do
  l <- slurpLine
  barfLine "-----"
  barfLine l
  kaikuupi

echo = do
  l <- getLine
  putStrLn "----"
  putStrLn l
  echo
  
interpretIO :: Free Terminal r -> IO () --to convert it back to the reference implementation and
interpretIO (Free thing) = case thing of
  SlurpLine f -> do
    a <- getLine 
    (interpretIO (f a))
  BarfLine s n-> putStrLn s
  Done -> return ()
  
   

mockIO :: Free Terminal r -> [String] -> [String] --to emulate the implementation without actually running it.
mockIO (Free thing) (x:commandLine) = case thing of
  SlurpLine f -> mockIO (f x) commandLine
  BarfLine s n-> mockIO n (s:(x:commandLine))
  Done -> (x:commandLine)



