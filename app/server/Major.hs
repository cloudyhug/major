module Major where

import API
import Matrix
import qualified Data.Map.Strict as M
import Data.List (maximumBy, intercalate)
import Control.Monad (void)
import System.Process (runCommand, waitForProcess)

type Map = M.Map

computeResults :: Matrix Int -> IO [CandidateScore]
computeResults voteData = return [] -- TODO

-- -- Given the collected ratings of each candidate, find the winner of the majority judgment election
-- computeWinner :: Map Name [Rating] -> Name
-- computeWinner = selectBestMedianGrade . fmap (computeMedianGrade . computeStatistics)

-- -- Given the median grade and proportion of each candidate, find the guy with the best data
-- selectBestMedianGrade :: Map Name (Rating, Double) -> Name
-- selectBestMedianGrade medianGrades = fst bestCandidate
--   where
--     medianGradesAssocs = M.assocs medianGrades
--     -- the ratings are ordered from best to worst, so best grade = minimum
--     maxMedianGrade = minimum $ map (fst . snd) medianGradesAssocs
--     maxCandidates = filter ((== maxMedianGrade) . fst . snd) medianGradesAssocs
--     bestCandidate = maximumBy (\(_, (_, p1)) (_, (_, p2)) -> compare p1 p2) maxCandidates

-- -- Given the proportion for each rating on a candidate, find the median grade
-- -- and the proportion of people that gave at least this grade to the candidate
-- computeMedianGrade :: Map Rating Double -> (Rating, Double)
-- computeMedianGrade = snd . M.foldlWithKey' addNext (False, (Reject, 0))
--   where
--     -- the boolean tells whether we have found the median grade
--     addNext acc@(True, _) _ _ = acc
--     addNext (False, (curR, curP)) r p =
--       if curP >= 0.5 then (True, (curR, curP)) else (False, (r, curP + p))

-- -- Given a list of ratings for a candidate, compute the proportion for each rating
-- computeStatistics :: [Rating] -> Map Rating Double
-- computeStatistics ratings = fmap (\r -> fromIntegral r / numOfRatings) countedRatings
--   where
--     emptyData = M.fromList $ fmap (\r -> (r, 0)) [Reject, Poor, Meh, Ok, Good, VeryGood, Excellent]
--     countedRatings = foldl (flip (M.adjust succ)) emptyData ratings 
--     numOfRatings = fromIntegral $ M.foldl' (+) 0 countedRatings

-- -- exports data for plotting
-- exportStatistics :: Map Name (Map Rating Double) -> String -> IO ()
-- exportStatistics stats filename = do
--   let statsToStrings = reverse . M.foldl' (\acc r -> show r : acc) []
--       electionEntryToString candidate hisStats =
--         "\"" ++ candidate ++ "\"\t" ++ intercalate "\t" (statsToStrings hisStats)
--       electionContent = "Grade\tExcellent\t\"Very Good\"\tGood\tOK\tMeh\tPoor\tReject\n" ++
--         (unlines $ M.foldlWithKey' (\acc c s -> electionEntryToString c s : acc) [] stats)
--   writeFile "election.dat" electionContent
--   let gnuplotGeneratorContent = unlines
--         [ "set terminal pngcairo enhanced font \"helvetica,14\" fontscale 1.0 size 1280, 720"
--         , "set output '" ++ filename ++ "'"
--         , "set border 3 front lt black linewidth 1.000 dashtype solid"
--         , "set boxwidth 0.75 absolute"
--         , "set style fill   solid 1.00 border lt -1"
--         , "set grid nopolar"
--         , "set grid noxtics nomxtics ytics nomytics noztics nomztics nortics nomrtics \\"
--         , " nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics"
--         , "set grid layerdefault lt 0 linecolor 0 linewidth 0.500," ++
--             " lt 0 linecolor 0 linewidth 0.500"
--         , "set key outside right top vertical Left reverse noenhanced" ++
--             " autotitle columnhead nobox"
--         , "set key invert samplen 4 spacing 1 width 0 height 0"
--         , "set style histogram rowstacked title textcolor lt -1"
--         , "set style data histograms"
--         , "set xtics border in scal 0,0 nomirror rotate by -45 autojustify"
--         , "set xtics norangelimit"
--         , "set xtics ()"
--         , "set ytics nomirror"
--         , "set title \"Majority Judgment Election Results\\n" ++
--             "Fraction of total plotted as stacked histogram\""
--         , "set ylabel \"% of total\""
--         , "NO_ANIMATION = 1"
--         , "set lt 1 lc rgb '#00b7e0'"
--         , "set lt 2 lc rgb '#54e972'"
--         , "set lt 3 lc rgb '#9fed67'"
--         , "set lt 4 lc rgb '#fff158'"
--         , "set lt 5 lc rgb '#ffd03a'"
--         , "set lt 6 lc rgb '#ff9100'"
--         , "set lt 7 lc rgb '#ff3700'"
--         , "set arrow 1 from -1,5000 to 12,5000 nohead front lc rgb '#6400ff' lw 3"
--         , "plot 'election.dat' using (100.*$2):xtic(1) t column(2)," ++
--             " for [i=3:8] '' using (100.*column(i)) title column(i)" ]
--   writeFile "generator.gnu" gnuplotGeneratorContent
--   void $ runCommand "gnuplot generator.gnu" >>= waitForProcess

