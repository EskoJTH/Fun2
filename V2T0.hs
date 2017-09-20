import Debug.Trace
--traceShow "kissa"
data ExerciseLevel = Nope|HonestAttempt|Correct|Awesome deriving (Eq,Ord,Show)
data Exercises = Exers[ExerciseLevel] deriving (Eq, Show)

getGrade :: Int -> Exercises -> String
getGrade credits exercises = computeGrade credits exercises 6

computeGrade :: Int -> Exercises -> Int -> String
computeGrade _ _ 0 = "Congartulations! Your grade is: Failed"
computeGrade credits exercises testGrade = if
  (computeGradeMore credits exercises exercises (grade testGrade) 0) then
  gradesTotext testGrade else
  computeGrade credits exercises (testGrade-1)

computeGradeMore :: Int -> Exercises -> Exercises -> [(ExerciseLevel,Int)] -> Int ->  Bool
computeGradeMore _ _ _ [] _ = True
computeGradeMore credits _ (Exers[])  (level:gradeLevel) acc
  |(acc >= (getWeeksFromCredits credits)*(snd level)) && (fst level == HonestAttempt || (0==snd((\(x:xs) -> x) gradeLevel)))= True
  |otherwise = False
computeGradeMore credits originEx (Exers(exr:exercises)) (level:gradeLevel) acc
  |(((fst level) == Awesome) && ((snd level) == 0)) = computeGradeMore credits (notAwesome (Exers []) originEx) (notAwesome (Exers []) originEx) (gradeLevel) 0
  |fst level == exr =computeGradeMore credits originEx (Exers(exercises)) (level:gradeLevel)(acc+1)
  |acc >=((getWeeksFromCredits credits) * (snd level)) = computeGradeMore credits originEx originEx (gradeLevel) 0
  |otherwise = computeGradeMore credits originEx (Exers(exercises)) (level:gradeLevel) acc

notAwesome (Exers(converted)) (Exers []) = (Exers(converted))
notAwesome (Exers(converted)) (Exers(exr:exercises))
  | exr == Awesome = notAwesome (Exers( Correct:Correct:converted)) (Exers exercises)
  | otherwise =  notAwesome (Exers( exr:converted)) (Exers exercises)
  
gradesTotext whatGrade = case whatGrade of
  6 -> "Awesome you got grade 5 and CAKEEEEEEE! ps. Cake might be a lie!" 
  5 -> "Awesome you got grade 5"
  4 -> "Awesome you got grade 4"
  3 -> "Awesome you got grade 3"
  2 -> "You got grade 2"
  1 -> "Well you passed"

  {-3 HA + 1 C = 1; 1HA + 2C = 2; 3C = 3; 3Ha + 3 C = 4;6C = 5; 3C + 3 A = 5 + Cakeee!!!-}
--requirements per week reversed so we cans start reading from the most valuable grade.
grade :: Int -> [(ExerciseLevel,Int)]
grade 1 = reverse [(HonestAttempt, 3),(Correct, 1),(Awesome,0)]
grade 2 = reverse [(HonestAttempt, 1),(Correct, 2),(Awesome,0)]
grade 3 = reverse [(HonestAttempt, 0),(Correct, 3),(Awesome,0)]
grade 4 = reverse [(HonestAttempt, 3),(Correct, 3),(Awesome,0)] 
grade 5 = reverse [(HonestAttempt, 0),(Correct, 6),(Awesome,0)]
grade 6 = reverse [(HonestAttempt, 0),(Correct, 3),(Awesome,3)]

getWeeksFromCredits :: Int -> Int
getWeeksFromCredits credits
  |credits == 1 = 2 
  |credits == 2 = 4
  |credits == 3 = 6
  |credits == 4 = 7
  |credits == 5 = 7 
  |otherwise = 2

test1 = getGrade 1 (Exers [Correct,Correct,Correct,Awesome,Awesome])--grade 3
test2 = getGrade 1 (Exers [HonestAttempt,HonestAttempt,HonestAttempt,HonestAttempt,HonestAttempt,HonestAttempt,Correct,Correct,Correct,Correct,Correct,Correct,Correct])--grade 4
test3 = getGrade 1 (Exers [Correct,Correct,Correct,Awesome,Awesome,Awesome,Correct,Correct,Correct,Correct,Correct,Correct,Correct])--grade 5
test4 = getGrade 1 (Exers [HonestAttempt,HonestAttempt,HonestAttempt,HonestAttempt,HonestAttempt,HonestAttempt,Correct,Correct,Correct])--grade 1
  
  
