module ScoresAndStatistics (
  stats, 
  typeStats,
  assignmentStats,
  score,
  typeScore,
  assignmentScore,
  ranked,
  typeRanked,
  assignmentRanked,
  UserScore(..),
  Bucket(..),
  Statistics(..)
) where

import Data.List
import Data.Char
import System.Random
import Data.Array.IO
import Control.Monad
import Data.Time
import System.IO
import Reviews
import AssignmentsAndSubmissions

type UserIdentifier = String
type Text = String
-- | Academic year shorthand (e.g. 2015 for 2015/16) 
type Year = Integer
--
-- | ToString functions for Assignment, ReviewAssignmet and Review.
-- | It concatinates the elements of those data structures with a tab character inbetween.
reviewToString (Review rev score text) = (reviewAssignmentToString rev) ++ "\t" ++ (show score) ++ "\t" ++ text
reviewAssignmentToString (ReviewAssignment er ee role zad) = er ++ "\t" ++ ee ++ "\t" ++ (show role) ++ "\t" ++ (assignmentToString zad)
assignmentToString (Assignment god tip n) = (show god) ++ "\t" ++ (show tip) ++ "\t" ++ (show n) 

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

--assignments = [ Assignment 2015 Exam 3, Assignment 2015 Exam 3, Assignment 2015 Exam 4]


-- | A "bucket" containing a count of values 
-- | in a certain range for plotting a histogram. 
data Bucket = Bucket { rangeMin :: Double , rangeMax :: Double , count :: Int } deriving Show
-- | A statistics container 
data Statistics = Statistics { minPossible :: Double 
                             , maxPossible :: Double 
                             , mean :: Double 
                             , median :: Double 
                             , minAchieved :: Double 
                             , maxAchieved :: Double 
                             , histogram :: [Bucket] 
                             } deriving Show
--
-- | A user’s score and accompanying data 
data UserScore = UserScore{ identifier :: UserIdentifier ,  points :: Double , passed :: Bool  } deriving (Eq, Ord, Show)

-- | Computes the statistics for an entire academic year 
stats :: [UserScore] -> IO Statistics
stats scores = do
        let pointss = sort $ map (\x -> points x) scores
        makeStatistics pointss
--
-- | Takes a list of points (Double), rangeMin, rangeMax, number of points(count) in that range
-- |  and a step(distance between rangeMin and rangeMax).
-- | Returns : a list of Bucket elements
-- | If the current point is in the given range it increases the count, and continues with the 
-- | elements of the points list, otherwise it creates a Bucket and appends it to the result list.
-- | Count is sat to 0, min to max, and max to (max + step).
makeHistogram [] min max count _ = [Bucket min max count]
makeHistogram a@(x:points) min max count step | x <= max = makeHistogram points min max (count +1) step
                                              | otherwise = (Bucket min max count): makeHistogram a max (max+step) 0 step
--
-- | Takes a list of scores (Double) and computes the necesary data for making 
-- | a Statistics variable. It assume the minPossible is 0, and maxPossible is 100
makeStatistics :: [Double] -> IO Statistics
makeStatistics [] = error "No statistics"
makeStatistics pointss = do 
        let size = length pointss
        let minAchived = pointss !! 0 
        let maxAchived = pointss !! (size-1)
        let mean = (sum $ map (\a@(x:xs) -> (x*(fromIntegral $ length a)/ (fromIntegral size))) (group pointss))
        -- if there is an even number of elements we take the middle two, sum and divide them by 2
        let median | mod size 2 /= 0 = pointss !! (div size 2)
                   | otherwise =((pointss !! (div size 2)) + (pointss !! ((div size 2)-1)))/2 
        let histogram = makeHistogram pointss 0.0 10.0 0 10
        return (Statistics 0 100 mean median minAchived maxAchived histogram)

-- | Computes the statistics for a certain assignment 
-- | type in an academic year. 
typeStats :: Integer -> Type -> IO Statistics
typeStats god tip = do
       file <- readFile "reviews.txt"
       let pom = filter (\(a:b:c:d:e:f:g:h) -> (d==(show god)) && ((show tip)==e)) (map words (lines file))
       let scores = map (\(a:b:c:d:e:f:g:h) -> (read g :: Double)) pom
       makeStatistics scores


-- | Computes the statistics for a certain assignment 
-- | (combination of AY, type and number)
assignmentStats :: Assignment -> IO Statistics
assignmentStats zad = do
        pom <- reviews zad
        let scores = map (\x -> scoreR x) pom
        makeStatistics scores

-- | Fetches the user score for an entire academic year 
-- | from the "reviews.txt" file. It filter the reviews that 
-- | contain the right year and UserIdentifier(as reviewee) and
-- | returns the UserScore.
score :: Integer -> UserIdentifier -> IO UserScore
score god id = do
       file <- readFile "reviews.txt"
       let pom = filter (\(a:b:c:d:e:f:g:h) -> (d==(show god)) && (b==id)) (map words (lines file))
       let score  = sum $ map (\(a:b:c:d:e:f:g:h) -> (read g :: Double)) pom
       return (UserScore id score (score>50))

-- | Fetches the user score for an assignment type in AY 
typeScore :: Integer -> Type -> UserIdentifier -> IO UserScore
typeScore god tip id = do
       file <- readFile "reviews.txt"
       let pom = filter (\(a:b:c:d:e:f:g:h) -> (d==(show god)) && (b==id) && ((show tip)==e)) (map words (lines file))
       let score  = sum $ map (\(a:b:c:d:e:f:g:h) -> (read g :: Double)) pom
       return (UserScore id score (score>50))

-- | Fetches the user score for a certain assignment 
assignmentScore :: Assignment -> UserIdentifier -> IO UserScore
assignmentScore zad id = do
       pom <- reviewsFor zad id
       let score = sum $ map (\x -> scoreR x) pom
       return (UserScore id score (score>50))

-- | Fetches the ranked users for an entire AY, sorted in 
-- | descending order by score. 
ranked :: Integer -> IO [UserScore]
ranked god = do
       file <- readFile "reviews.txt"
       let reviews = filter (\(a:b:c:d:e:f:g:h) -> d==(show god)) (map words (lines file))
       let scores = groupBy (\a b-> (fst a) == (fst b)) $ sort $ map (\(a:b:c:d:e:f:g:h) -> (b, (read g :: Double))) reviews
       let userScores = map (\a@(x:xs) -> UserScore (fst x) (sumUserScores a 0) ((sumUserScores a 0)>50)) scores
       return $ sortBy (\a b -> compare (points b) (points a)) userScores
--
-- | Gets a list of tuples (userIdentifier, Double) where all the userIdentifiers are the same
-- | It sums the double values
sumUserScores [] score = score
sumUserScores (x:xs) score = sumUserScores xs (score+(snd x))

-- | Fetches the ranked users for an assignment type in AY, 
-- | sorted in descending order by score. 
typeRanked :: Integer -> Type -> IO [UserScore]
typeRanked god tip = do
       file <- readFile "reviews.txt"
       let reviews = filter (\(a:b:c:d:e:f:g:h) -> (d==(show god)) && ((show tip)==e)) (map words (lines file))
       let scores = groupBy (\a b-> (fst a) == (fst b)) $ sort $ map (\(a:b:c:d:e:f:g:h) -> (b, (read g :: Double))) reviews
       let userScores =  map (\a@(x:xs) -> UserScore (fst x) (sumUserScores a 0) ((sumUserScores a 0)>50)) scores
       return $ sortBy (\a b -> compare (points b) (points a)) userScores

-- | Fetches the ranked users for a specific assignment, sorted 
-- | in descending order by score. 
assignmentRanked :: Assignment -> IO [UserScore]
assignmentRanked zad = do
       pom <- reviews zad
       let scores = groupBy (\a b-> (fst a) == (fst b)) $ sort $ map (\x -> (reviewee $ reviewAssignment x, scoreR x)) pom
       let userScores =  map (\a@(x:xs) -> UserScore (fst x) (sumUserScores a 0) ((sumUserScores a 0)>50)) scores
       return $ sortBy (\a b -> compare (points b) (points a)) userScores












