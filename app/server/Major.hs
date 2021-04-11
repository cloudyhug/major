module Major where

import API (CandidateScore(CandidateScore), Rating(Excellent))
import Matrix (Matrix(Matrix), elemsMatrix)
import Data.List (sortBy)

computeResults :: Matrix Int -> Int -> IO [CandidateScore]
computeResults voteData@(Matrix (n, m) content) numberVotesCast =
  sortBy (flip compare) . zipWith computeCandidateScore [1..n] <$> elemsMatrix voteData
    where
      numberVotesCastFlt = fromIntegral numberVotesCast
      fiftyPercentOfVotesCastFlt = fromIntegral numberVotesCast / 2.0

      computeCandidateScore :: Int -> [Int] -> CandidateScore
      computeCandidateScore scoreId row =
        uncurry (CandidateScore scoreId) $ findMedian Excellent 0 row

      findMedian :: Rating -> Int -> [Int] -> (Rating, Double)
      findMedian nextRating accumulatedScore (score : scores) =
        let newAccumulatedScore = accumulatedScore + score
            newAccumulatedScoreFlt = fromIntegral newAccumulatedScore in
        if newAccumulatedScoreFlt >= fiftyPercentOfVotesCastFlt then
          (nextRating, newAccumulatedScoreFlt / numberVotesCastFlt)
        else
          findMedian (succ nextRating) newAccumulatedScore scores

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

