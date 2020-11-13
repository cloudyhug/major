module Major where

import Common
import qualified Data.Map.Strict as M
import Data.List (maximumBy)

type Map = M.Map

-- Given the collected ratings of each candidate, find the winner of the majority judgment election
computeWinner :: Map Name [Rating] -> Name
computeWinner = selectBestMedianGrade . fmap (computeMedianGrade . computeStatistics)

-- Given the median grade and proportion of each candidate, find the guy with the best data
selectBestMedianGrade :: Map Name (Rating, Double) -> Name
selectBestMedianGrade medianGrades = fst bestCandidate
  where
    medianGradesAssocs = M.assocs medianGrades
    -- the ratings are ordered from best to worst, so best grade = minimum
    maxMedianGrade = minimum $ map (fst . snd) medianGradesAssocs
    maxCandidates = filter ((== maxMedianGrade) . fst . snd) medianGradesAssocs
    bestCandidate = maximumBy (\(_, (_, p1)) (_, (_, p2)) -> compare p1 p2) maxCandidates

-- Given the proportion for each rating on a candidate, find the median grade
-- and the proportion of people that gave at least this grade to the candidate
computeMedianGrade :: Map Rating Double -> (Rating, Double)
computeMedianGrade = snd . M.foldlWithKey' addNext (False, (Reject, 0))
  where
    -- the boolean tells whether we have found the median grade
    addNext acc@(True, _) _ _ = acc
    addNext (False, (curR, curP)) r p =
      if curP >= 0.5 then (True, (curR, curP)) else (False, (r, curP + p))

-- Given a list of ratings for a candidate, compute the proportion for each rating
computeStatistics :: [Rating] -> Map Rating Double
computeStatistics ratings = fmap (\r -> fromIntegral r / numOfRatings) countedRatings
  where
    emptyData = M.fromList $ fmap (\r -> (r, 0)) [Reject, Poor, Meh, Ok, Good, VeryGood, Excellent]
    countedRatings = foldl (flip (M.adjust succ)) emptyData ratings 
    numOfRatings = fromIntegral $ M.foldl' (+) 0 countedRatings
