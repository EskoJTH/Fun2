import Data.Time.Clock
import Data.Time.Calendar
import Data.Semigroup as Semi
import Data.Monoid as M
import Data.Foldable
import Data.List
import GHC.Exts

data Vesa =
  Vesa {project:: String, subproject :: String, starttime :: UTCTime, endtime :: UTCTime, comment :: Comment} deriving (Eq,Show)

data Answer =
  Answer {projectName :: Name, timeSpent :: TimeSpent, timeSpan :: TimeSpan, countForTime :: Count, commment :: Comment} deriving (Eq,Show)
instance Monoid Answer where
  mempty = Answer mempty mempty mempty mempty mempty
  mappend (Answer p t1 t2 avgt c) (Answer p' t1' t2' avgt' c') = Answer (p M.<> p') (t1 M.<>t1') (t2 M.<> t2') (avgt M.<>avgt') (c M.<>c')

newtype Name = Name {s ::String} deriving (Show,Eq)
instance Monoid Name where
  mempty = Name ""
  mappend (Name a) (Name b)
    |a==b = Name a
    |otherwise = Name (a ++ b)

newtype TimeSpent = TimeSpent {getTimediff ::  NominalDiffTime} deriving (Eq,Show,Ord)
instance Monoid TimeSpent where
  mempty = TimeSpent 0
  mappend (TimeSpent a) (TimeSpent b) = TimeSpent (a + b)

newtype Count = Count{count :: NominalDiffTime} deriving (Eq,Show,Ord)
instance Monoid Count where
  mempty = Count 0
  mappend (Count a) (Count b) = Count((max a b)+1)
  
newtype TimeSpan = TimeSpan {getTime :: Maybe (UTCTime,UTCTime)} deriving (Eq,Show,Ord)
instance Monoid TimeSpan where
  mempty = TimeSpan Nothing
  mappend (TimeSpan Nothing) (TimeSpan (Just(c,d))) = TimeSpan (Just (c,d))
  mappend (TimeSpan (Just (a,b))) (TimeSpan Nothing) = TimeSpan (Just (a,b))
  mappend (TimeSpan (Just (a,b))) (TimeSpan (Just (c,d))) = TimeSpan (Just (min a c, max b d))
  
newtype Comment = Comment {getComment :: String} deriving (Eq,Show,Ord)
instance Monoid Comment  where
  mempty = Comment []
  mappend (Comment a) (Comment b) = Comment (a ++ " " ++ b)
  
--data Vesat a = Vesat {a ::[Vesa]} deriving (Eq,Show)
--instance Monoid (Vesat a) where
--  mempty = Vesat []
--  mappend (Vesat a) (Vesat b) = Vesat (a ++ b)

--tulosta projektit = tulostaAnswers (kasiteleProjektit projektit)
--tulostaAnswers x:xs = 
--tulostaAnswer vastaukset=
jarjesta :: [Vesa] ->[[Vesa]]
jarjesta vesat = groupBy groupHelper (sortWith (\(Vesa a b c d e)->a) vesat)

groupHelper (Vesa a b c d e) (Vesa a' b' c' d' e') = if(a==(a'))then True else False

kasiteleProjektit :: [[Vesa]]->[Answer]
kasiteleProjektit projektit =map (convertter) projektit

convertter :: [Vesa]->Answer
convertter vesat = foldMap (convert) vesat
convert :: Vesa->Answer
convert (Vesa p1 p2 start end com) =
  Answer (Name p1) (TimeSpent (diffUTCTime end start)) (TimeSpan (Just(start,end))) (Count(0)) (com)

--kasiteleProjektit' :: [[Vesa]]->[Answer]
--kasiteleProjektit' (projekti:projektit) = (convertter projekti):kasiteleProjektit(projektit)

--convertter' :: [Vesa]->Answer
--convertter' [(Vesa p1 p2 start end com)] =
--  Answer (Name p1) (TimeSpent (diffUTCTime start end)) (TimeSpan (Just(start,end))) (Count(diffUTCTime start end)) (com)
--convertter' (vesa:vesat) =(convertter [vesa]) M.<> (convertter vesat)

--{projectName :: Name, timeSpent :: TimeSpent, timeSpan :: TimeSpan, avgtime :: Count, commment :: Comment} deriving (Eq,Show)
tulosta :: [Answer]->String
tulosta vastaukset = concat (map tulostaYksAnswer vastaukset)
tulostaYksAnswer :: Answer -> String
tulostaYksAnswer (Answer (Name p) (TimeSpent t1) (TimeSpan(Just t2)) (Count avgt) (Comment c)) = "||||Project:" ++ (p ++("-----Total time used:"++(show t1 ++ ("-----Total time span:" ++ (show t2 ++ ("-----Average time spent on project in one sitting:" ++ (show( t1/avgt) ++ ("-----Comments:" ++   c ++("||||")))))))))

--86400
testiData =  [(Vesa "elaimet" "kissa" (UTCTime (ModifiedJulianDay 70000) (secondsToDiffTime 10000)) (UTCTime (ModifiedJulianDay 70000) (secondsToDiffTime 20000))(Comment "Kissa ohjelmoitiin istumaan nappaimmistolla.")),
              (Vesa "elaimet" "koira" (UTCTime (ModifiedJulianDay 70000) (secondsToDiffTime 40000)) (UTCTime (ModifiedJulianDay 70000) (secondsToDiffTime 50000)) (Comment "Hau Hau!")),
              (Vesa "Palindromi" "Saippuakauppias" (UTCTime (ModifiedJulianDay 70500) (secondsToDiffTime 36400)) (UTCTime (ModifiedJulianDay 70500) (secondsToDiffTime 40000))(Comment "Saippua- eli vuolukivea tyostava Tulikivi-yritys on saippuakivikauppias -Wikipedia.")),
              (Vesa "Palindromi" "Ajokkilikkoja" (UTCTime (ModifiedJulianDay 70500) (secondsToDiffTime 50400)) (UTCTime (ModifiedJulianDay 70501) (secondsToDiffTime 30000))(Comment "Palondromit ovat ohjelmointikielen tarkein String tyypin metodi."))]

vastaus = tulosta $ kasiteleProjektit $ jarjesta testiData

--laske :: [Vesa]->String
--laske ((Vesa p1 p2 start end com):projektit) = 

--((ToistottomatVesat a b c d e):rest) ((ToistottomatVesat a' b' c' d' e'):rest')
--tuolosta vesat = FoldMap (\(Vesa a b c d e) (Vesa a b c d e)->()) (Vesat a b c d e) mappend

--tulosta vesat = mconcat(map katto
  
--foldMap :: (Foldable f, Monoid m) => (a -> m) -> f a -> m

-- tulosta :: Vesat->String
-- tulosta vesat =
-- 
--Kuinka voin kiert‰‰ rekursion t‰ss‰?
--
