{-# LANGUAGE LambdaCase #-}
module API where

import Control.Monad.Extra (unless, unlessM, whenM)
import Control.Monad.Trans.Except (throwE)
import Data.List (sortOn, partition, intercalate)
import Data.Ord (Down(Down))

import Domain
import Data.Bifunctor (bimap)
import Text.Printf (printf)
import Data.Tuple.Extra (both)

connect :: Database db => db -> Credentials -> AppM ConnectionInfo
connect database cred@(Credentials login password clientID) = atomicallyWith database $ \db -> do
  unless (isAcceptableCredentials cred) $ throwE UnacceptableCredentials
  unlessM (isValidSession db login clientID) $ throwE InvalidSession
  unlessM (isValidCredentials db login password) $ throwE InvalidCredentials
  getTokens db login clientID

register :: Database db => db -> Credentials -> AppM ()
register database cred@(Credentials login password clientID) = atomicallyWith database $ \db -> do
  unless (isAcceptableCredentials cred) $ throwE UnacceptableCredentials
  whenM (existsUser db login) $ throwE UserAlreadyRegistered
  whenM (existsClient db clientID) $ throwE ClientAlreadyRegistered
  addUser db login password clientID

isAcceptableCredentials :: Credentials -> Bool
isAcceptableCredentials (Credentials login password clientID) =
  not $ any null [login, password, clientID]

renewToken :: Database db => db -> RefreshToken -> AppM AccessToken
renewToken database refreshToken = atomicallyWith database $ \db -> do
  getNewAccessToken db refreshToken >>= \case
    Just accessToken -> return accessToken
    Nothing -> throwE InvalidRefreshToken

getElections :: Database db => db -> AccessToken -> AppM [ElectionInfo]
getElections database accessToken = atomicallyWith database $ \db -> do
  unlessM (isValidAccessToken db accessToken) $ throwE InvalidAccessToken
  findAllElections db

getElectionInfo :: Database db => db -> AccessToken -> ElectionID -> AppM Election
getElectionInfo database accessToken electionID = atomicallyWith database $ \db -> do
  unlessM (isValidAccessToken db accessToken) $ throwE InvalidAccessToken
  unlessM (existsElection db electionID) $ throwE ElectionNotFound
  findElectionDataByID db electionID

getElectionResults :: Database db => db -> AccessToken -> ElectionID -> AppM ElectionResults
getElectionResults database accessToken electionID = atomicallyWith database $ \db -> do
  unlessM (isValidAccessToken db accessToken) $ throwE InvalidAccessToken
  unlessM (existsElection db electionID) $ throwE ElectionNotFound
  unlessM (isFinishedElection db electionID) $ throwE ElectionStillRunning
  findElectionResultsByID db electionID >>= \case
    Just results -> return results
    Nothing -> do
      stats <- findElectionStatsByID db electionID
      case computeResults stats of
        Nothing -> throwE NoVoteCast
        Just results -> do
          addElectionResults db electionID results
          return results

computeResults :: [(Candidate, [(Rating, Int)])] -> Maybe ElectionResults
computeResults [] = Nothing
computeResults stats =
  -- first, run majority judgment
  let
    sortedStats = map (bimap (\(Candidate _ name _) -> name) (sortOn Down)) stats
    sortedStatsWithMedianGrades = sortOn (Down . snd4) $ map addMedianGrade sortedStats
    bestMedianGrade = snd4 $ head sortedStatsWithMedianGrades
    (bestCandidates, eliminatedCandidates) =
      span ((== bestMedianGrade) . snd4) sortedStatsWithMedianGrades
  in
    if length bestCandidates == 1 then -- only one winner, computation is done
      let
        (winner, medianGrade, medianScore, _) = head bestCandidates
        explanation = printf msg1 winner (show medianGrade) (show medianScore)
      in
        Just $ ElectionResults explanation (map init4 sortedStatsWithMedianGrades)
    else
      -- must break the tie, we do *one* run of usual judgment
      let
        bestCandidatesWithNScore = sortOn fst $ map ((,) =<< computeNScore) bestCandidates
        (nScore, (winner, medianGrade, _, _)) = head bestCandidatesWithNScore
        reSortedStatsWithMedianGrades = map snd bestCandidatesWithNScore ++ eliminatedCandidates
        readjustedMedianGrades = map readjustMedianGrade bestCandidatesWithNScore
        formattedWinnerReadjustedMedianGrade = printf "%s (%s)" winner nScore :: String
        formattedOther = intercalate ", " $ map (uncurry (printf "%s (%s)")) readjustedMedianGrades
        explanation = printf msg2
          (intercalate ", " $ map fst4 bestCandidates) (show bestMedianGrade) winner
          formattedWinnerReadjustedMedianGrade formattedOther
      in
        Just $ ElectionResults explanation (map init4 reSortedStatsWithMedianGrades)

    where
      nbVotesCast = fromIntegral . sum . map snd . snd $ head stats

      -- median grade from majority judgment
      addMedianGrade :: (String, [(Rating, Int)]) -> (String, Rating, Double, [(Rating, Int)])
      addMedianGrade (candidate, scores) =
        let (medianGrade, medianScore) = computeMedianGrade scores in
          (candidate, medianGrade, medianScore, scores)
      computeMedianGrade :: [(Rating, Int)] -> (Rating, Double)
      computeMedianGrade = go Excellent 0.0 . map (fromIntegral . snd)
        where
          go currentGrade accumulatedScore scores =
            let accumulatedScore' = accumulatedScore + head scores in
              if accumulatedScore' >= nbVotesCast / 2.0 then
                (currentGrade, accumulatedScore' / nbVotesCast * 100.0)
              else
                go (pred currentGrade) accumulatedScore' $ tail scores

      -- n score from usual judgment
      computeNScore :: (String, Rating, Double, [(Rating, Int)]) -> Double
      computeNScore (name, medianGrade, _, scores) =
        let
          (betterScores, worseScores) =
            partition ((> medianGrade) . fst) $ filter ((/= medianGrade) . fst) scores
          (p, q) = both ((/ nbVotesCast) . fromIntegral . sum . map snd) (betterScores, worseScores)
        in
          0.5 * (p - q) / (1 - p - q)

      -- formatting and util
      readjustMedianGrade :: (Double, (String, Rating, Double, [(Rating, Int)])) -> (String, String)
      readjustMedianGrade (nScore, (name, medianGrade, _, _)) =
        (name, printf "%s %+.3f" (show medianGrade) nScore)
      msg1 =
        "%s is the winner, being the only candidate with median grade %s. " ++
        "%.2f%% of the voters rated this candidate with at least this median grade."
      msg2 = concat
        [ "The best candidates are %s, who all obtained the median grade %s. ",
          "To break the tie, we used the method of usual judgment: a special \"N score\" was ",
          "computed for each of the aforementioned candidates, taking into account the share of ",
          "people who assigned them a better grade than their median grade, people who assigned ",
          "them a worse grade than their median grade, and the share of people who assigned ",
          "them their median grade. The ratings were readjusted, and the winner is %s, with the ",
          "final grade %s. The other readjusted grades are the following: %s." ]
      fst4 (a, _, _, _) = a
      snd4 (_, b, _, _) = b
      init4 (a, b, c, _) = (a, b, c)

getVote :: Database db => db -> AccessToken -> ElectionID -> AppM Ballot
getVote database accessToken electionID = atomicallyWith database $ \db -> do
  unlessM (isValidAccessToken db accessToken) $ throwE InvalidAccessToken
  unlessM (existsElection db electionID) $ throwE ElectionNotFound
  user <- findUserByToken db accessToken
  findVoteByUser db electionID user >>= \case
    Nothing -> throwE UserDidNotVote
    Just ballot -> return ballot

vote :: Database db => db -> AccessToken -> ElectionID -> Ballot -> AppM ()
vote database accessToken electionID ballot = atomicallyWith database $ \db -> do
  unlessM (isValidAccessToken db accessToken) $ throwE InvalidAccessToken
  unlessM (existsElection db electionID) $ throwE ElectionNotFound
  whenM (isFinishedElection db electionID) $ throwE ElectionNotRunning
  user <- findUserByToken db accessToken
  findVoteByUser db electionID user >>= \case
    Just _ -> throwE UserAlreadyVoted
    Nothing -> addVote db electionID user ballot

endElection :: Database db => db -> AccessToken -> ElectionID -> AppM ()
endElection database accessToken electionID = atomicallyWith database $ \db -> do
  unlessM (isAdminToken db accessToken) $ throwE UserIsNotAdmin
  unlessM (existsElection db electionID) $ throwE ElectionNotFound
  whenM (isFinishedElection db electionID) $ throwE ElectionNotRunning
  setFinishedElection db electionID
