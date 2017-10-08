module SimpleEffectLanguage where
import Control.Monad.Free --no mitäh ..........

data Terminal next =
  SlurpLine (String -> next) |
  BarfLine String next |
  Done
  deriving Functor

slurpLine :: Free Terminal String --as the counterpart of getLine,
slurpLine = undefined
barfLine :: String -> Free Terminal () --as the counterpart of putStrLn and
barfLine = undefined
terminate :: Free Terminal () --as the counterpart of pure ()
terminate = undefined

echo = do
  l <- getLine
  putStrLn "----"
  putStrLn l
  echo

  interpretIO :: Free Terminal r -> IO () --to convert it back to the reference implementation and
mockIO :: Free Terminal r -> [String] -> [String] --to emulate the implementation without actually running it.
