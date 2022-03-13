{-# LANGUAGE LambdaCase #-}
module API where

import Control.Monad.Extra (unless, unlessM, whenM)
import Control.Monad.Trans.Except (throwE)
import Data.List (sortOn)
import Data.Ord (Down(Down))

import Domain

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
  Just . ElectionResults . sortOn (Down . dropFst3) $ map computeCandidateResult stats
    where
      nbVotesCast = fromIntegral . sum . map snd . snd $ head stats
      dropFst3 (_, x, y) = (x, y)
      computeCandidateResult (Candidate _ name _, candidateStats) =
        cons2 name . go Excellent 0.0 . map (fromIntegral . snd) $ sortOn Down candidateStats
          where
            cons2 x (y, z) = (x, y, z)
            go grade accumulatedScore scores =
              let accumulatedScore' = accumulatedScore + head scores in
                if accumulatedScore' >= nbVotesCast / 2.0 then
                  (grade, accumulatedScore' / nbVotesCast * 100.0)
                else
                  go (pred grade) accumulatedScore' $ tail scores

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
